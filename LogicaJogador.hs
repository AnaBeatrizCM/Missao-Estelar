module LogicaJogador (
    naveInicial, moverNave, imagemNave, naveTiro, moverTiros, atualizarEstado
) where

import Data.Maybe (mapMaybe)
import Graphics.Gloss
import Tipos  -- Importa os tipos compartilhados

-- Imagem da nave
imagemNave :: Picture -> Nave -> Picture
imagemNave img (Nave x y _ _) = Translate x y (Scale 0.3 0.3 img)

-- Estado inicial da Nave
naveInicial :: Nave
naveInicial = Nave 0 (-alturaJanela/2 + raioNave) 10 3  -- Posição Y ajustada

-- Movimento da nave
moverNave :: Nave -> Float -> Nave
moverNave nave@(Nave posX posY vel hp) direcao
    | hp <= 0 = nave
    | novaPosicao < (-larguraJanela/2 + raioNave) = Nave (-larguraJanela/2 + raioNave) posY vel hp
    | novaPosicao > (larguraJanela/2 - raioNave) = Nave (larguraJanela/2 - raioNave) posY vel hp
    | otherwise = Nave novaPosicao posY vel hp
    where
        novaPosicao = posX + (vel * direcao)

-- Imagem do Tiro
imagemTiro :: Tiro -> Picture
imagemTiro (Tiro x y _) = Translate x y (Color red (rectangleSolid 5 15))

-- Combinar nave e tiro
naveTiro :: Picture -> EstadoJogador -> Picture
naveTiro imgNave (EstadoJogador nave tiros) =
    Pictures (imagemNave imgNave nave : map imagemTiro tiros)

-- Movimento do Tiro e remoção caso saia da tela
moverTiro :: Tiro -> Maybe Tiro
moverTiro (Tiro x y vel)
    | novaPosicaoY > alturaJanela / 2 = Nothing  -- Remove o tiro se ele sair da tela
    | otherwise = Just (Tiro x novaPosicaoY vel)
    where
        novaPosicaoY = y + vel  -- Movendo para cima

-- Reduz a vida do jogador quando atingido
tomarDano :: Nave -> Nave
tomarDano (Nave x y vel hp)
    | hp > 0 = Nave x y vel (hp-1)
    | otherwise = Nave x y vel 0

-- Verifica se o jogador ainda está vivo
jogadorVivo :: Nave -> Bool
jogadorVivo (Nave _ _ _ hp) = hp > 0

-- Verifica se um tiro inimigo atingiu a nave
colideComTiro :: Nave -> Tiro -> Bool
colideComTiro (Nave x y _ _) (Tiro tx ty _) =
    abs (x - tx) < raioNave && ty >= y && ty <= (y + raioNave)

-- Verifica se algum tiro inimigo atingiu a nave
verificarColisoes :: Nave -> [Tiro] -> Nave
verificarColisoes nave tiros
    | any (colideComTiro nave) tiros = tomarDano nave
    | otherwise = nave

-- Atirar
atirar :: EstadoJogador -> EstadoJogador
atirar estado@(EstadoJogador nave tiros)
    | jogadorVivo nave = EstadoJogador nave (novoTiro : tiros)
    | otherwise = estado
    where
        novoTiro = Tiro (posicaoX nave) (posicaoY nave + raioNave-10) 7  -- Tiro sai da nave e sobe

-- Mover tiros
moverTiros :: EstadoJogador -> EstadoJogador
moverTiros (EstadoJogador nave tiros) = EstadoJogador nave tirosAtualizados
    where
        tirosAtualizados = mapMaybe moverTiro tiros

-- Atualizar estado do jogo - Move os tiros a cada frame
atualizarEstado :: Float -> EstadoJogador -> EstadoJogador
atualizarEstado _ = moverTiros
