module LogicaJogador (
    naveInicial, moverNave, imagemNave, naveTiro, moverTiros, atualizarEstado,
    jogadorVivo, verificarColisoes, estadoInicialJogo
) where

import Data.Maybe (mapMaybe)
import Graphics.Gloss
import Tipos
import LogicaInvasores (estadoInicialInvasores, moverInvasores, verificarColisoesTirosInvasores)
import Debug.Trace

imagemNave :: Picture -> Nave -> Picture
imagemNave img (Nave x y _ _) = Translate x y (Scale 0.3 0.3 img)

naveInicial :: Nave
naveInicial = Nave 0 (-alturaJanela/2 + raioNave) 10 3

estadoInicialJogo :: EstadoJogador
estadoInicialJogo = EstadoJogador naveInicial [] estadoInicialInvasores 

imagemInvasor :: Picture -> Invasor -> Picture
imagemInvasor img (Invasor x y _) = Translate x y (Scale 0.2 0.2 img)

naveTiro :: Picture -> Picture -> EstadoJogador -> Picture
naveTiro imgNave imgInvasores estado@(EstadoJogador nave tiros estadoInvasores) =
    let invasoresAtuais = invasores estadoInvasores
    in Pictures ( imagemNave imgNave nave
                   : map imagemTiro tiros
                   ++ map (imagemInvasor imgInvasores) invasoresAtuais)

moverNave :: Nave -> Float -> Nave
moverNave nave@(Nave posX posY vel hp) direcao
    | hp <= 0 = nave
    | novaPosicao < (-larguraJanela/2 + raioNave) = Nave (-larguraJanela/2 + raioNave) posY vel hp
    | novaPosicao > (larguraJanela/2 - raioNave) = Nave (larguraJanela/2 - raioNave) posY vel hp
    | otherwise = Nave novaPosicao posY vel hp
    where
        novaPosicao = posX + (vel * direcao)

imagemTiro :: Tiro -> Picture
imagemTiro (Tiro x y _) = Translate x y (Color red (rectangleSolid 5 15))

moverTiro :: Tiro -> Maybe Tiro
moverTiro (Tiro x y vel)
    | novaPosicaoY > alturaJanela / 2 = Nothing
    | otherwise = Just (Tiro x novaPosicaoY vel)
    where
        novaPosicaoY = y + vel

tomarDano :: Nave -> Nave
tomarDano (Nave x y vel hp)
    | hp > 0 = Nave x y vel (hp-1)
    | otherwise = Nave x y vel 0

jogadorVivo :: Nave -> Bool
jogadorVivo (Nave _ _ _ hp) = hp > 0

colideComTiro :: Nave -> Tiro -> Bool
colideComTiro (Nave x y _ _) (Tiro tx ty _) =
    abs (x - tx) < raioNave && ty >= y && ty <= (y + raioNave)

verificarColisoes :: Nave -> [Tiro] -> Nave
verificarColisoes nave tiros
    | any (colideComTiro nave) tiros = tomarDano nave
    | otherwise = nave

atirar :: EstadoJogador -> EstadoJogador
atirar estado@(EstadoJogador nave tiros invasores)
    | jogadorVivo nave = EstadoJogador nave (novoTiro : tiros) invasores
    | otherwise = estado
    where
        novoTiro = Tiro (posicaoX nave) (posicaoY nave + raioNave-10) 7

moverTiros :: EstadoJogador -> EstadoJogador
moverTiros (EstadoJogador nave tiros invasores) = EstadoJogador nave tirosAtualizados invasores
    where
        tirosAtualizados = mapMaybe moverTiro tiros

atualizarEstado :: Float -> EstadoJogador -> EstadoJogador
atualizarEstado dt estado@(EstadoJogador nave tiros estadoInvasoresVal) =  -- Renomeie a variável aqui
    let estadoComTirosAtualizados = moverTiros estado
        estadoComInvasoresMovidos = estadoComTirosAtualizados {
            estadoInvasores = moverInvasores dt (estadoInvasores estadoComTirosAtualizados)  -- Acesse via função
        }
        (tirosRestantes, invasoresRestantes) = verificarColisoesTirosInvasores 
            (tirosJogador estadoComInvasoresMovidos) 
            (invasores (estadoInvasores estadoComInvasoresMovidos))  -- Acesso correto
        estadoComInvasoresAtualizados = estadoComInvasoresMovidos {
            tirosJogador = tirosRestantes,
            estadoInvasores = (estadoInvasores estadoComInvasoresMovidos) { invasores = invasoresRestantes }  -- Corrigido
        }
    in estadoComInvasoresAtualizados
