module LogicaJogador where
import Data.Maybe (mapMaybe)

-- Definicao das constantes (limites) da tela
larguraJanela, alturaJanela:: Float
larguraJanela = 800
alturaJanela = 600

-- Largura da Nave (a ser definido depois os pixels )
larguraNave :: Float
larguraNave = 80


-- Raio de colisão da nave (aproximado)
raioNave:: Float
raioNave = larguraNave / 2

-- Tipo para representar a nave
data Nave = Nave {
    posicaoX :: Float,
    velocidade :: Float,
    vida :: Int
} deriving (Show)


-- Estado inicial da Nave
naveInicial:: Nave
naveInicial = Nave (larguraJanela / 2) 10 3 -- Começa no meio da tela com 3 vidas


-- Movimento da nave, limitado dentro da tela
moverNave:: Nave -> Float -> Nave
moverNave nave@(Nave posX vel hp) direcao
    | hp <= 0 = nave -- se morreu, nao se move mais
    | novaPosicao < 0 = Nave 0 vel hp -- Limite Esquerdo
    | novaPosicao > larguraJanela = Nave larguraJanela vel hp -- Limite Direito
    | otherwise = Nave novaPosicao vel hp
    where
        novaPosicao = posX + (vel * direcao)


-- Tipo para representar um tiro
data Tiro = Tiro {
    posicaoTiroX:: Float,
    posicaoTiroY:: Float,
    velocidadeTiro:: Float
} deriving (Show)


-- Movimento do Tiro e remocao caso saia da tela
moverTiro:: Tiro -> Maybe Tiro
moverTiro (Tiro x y vel)
    | novaPosicaoY < 0 = Nothing -- Remove o tiro se sair da tela
    | otherwise = Just (Tiro x novaPosicaoY vel)
    where
        novaPosicaoY = y - vel

-- Reduz a vida do jogador quando atingido
tomarDano:: Nave -> Nave
tomarDano (Nave x vel hp)
    | hp > 0 = Nave x vel (hp-1)
    | otherwise = Nave x vel 0


-- Verifica se o jogador ainda esta vivo
jogadorVivo:: Nave -> Bool
jogadorVivo(Nave _ _ hp) = hp > 0

-- Verifica se um tiro inimigo atingiu a nave (colisão)
-- aqui considerei a altura da nave sendo 50, como o eixo Y comeca de [0..600], entao ty em 550 ou mais
-- ja tem atingido a nave
colideComTiro:: Nave -> Tiro -> Bool
colideComTiro (Nave x _ _) (Tiro tx ty _) 
    = abs(x - tx) < raioNave && ty >= 550


-- Verifica se algum tiro inimigo atingiu a nave
verificarColisoes:: Nave -> [Tiro] -> Nave
verificarColisoes nave tiros
    | any (colideComTiro nave) tiros = tomarDano nave
    | otherwise = nave

-- Tipo para armazenar o estado do jogador 
data EstadoJogador = EstadoJogador {
    nave :: Nave,
    tirosJogador :: [Tiro] -- Lista de tiros ativos
} deriving (Show)


atirar:: EstadoJogador -> EstadoJogador
atirar estado@(EstadoJogador nave tiros)
    |jogadorVivo nave = EstadoJogador nave (novoTiro: tiros) -- so dispara se estiver vivo
    |otherwise = estado
    where
        novoTiro = Tiro(posicaoX nave) 550 7 -- Tiro comeca a partir da pos. 550 no eixo Y e com velocidade fixa 7


moverTiros:: EstadoJogador -> EstadoJogador
moverTiros (EstadoJogador nave tiros) = EstadoJogador nave tirosAtualizados
    where
        tirosAtualizados = mapMaybe moverTiro tiros





main = do
    let estadoInicial = EstadoJogador naveInicial [] -- Sem tiros no começo
    print estadoInicial
    
    -- O jogador atira duas vezes
    let estado1 = atirar estadoInicial
    let estado2 = atirar estado1
    
    print estado2 -- Agora a lista tem 2 tiros
    
    -- Movemos os tiros duas vezes
    let estado3 = moverTiros estado2
    let estado4 = moverTiros estado3
    
    print estado4 -- Os tiros foram para cima!



