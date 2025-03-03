module Tipos (
    larguraJanela, alturaJanela,
    larguraNave, raioNave,
    Nave(..), Tiro(..), EstadoJogador(..),
    Invasor(..), EstadoInvasores(..)
) where

-- Definição das constantes (limites) da tela
larguraJanela, alturaJanela :: Float
larguraJanela = 400
alturaJanela = 600

-- Largura da Nave
larguraNave :: Float
larguraNave = 80

-- Raio de colisão da nave (aproximado)
raioNave :: Float
raioNave = larguraNave / 2

-- Tipo para representar a nave
data Nave = Nave {
    posicaoX :: Float,
    posicaoY :: Float,  -- Novo campo: posição Y da nave
    velocidade :: Float,
    vida :: Int
} deriving (Show)

-- Tipo para representar um tiro
data Tiro = Tiro {
    posicaoTiroX :: Float,
    posicaoTiroY :: Float,
    velocidadeTiro :: Float
} deriving (Show)

-- Tipo para armazenar o estado do jogador
data EstadoJogador = EstadoJogador {
    nave :: Nave,
    tirosJogador :: [Tiro]
} deriving (Show)

-- Tipo para representar um invasor
data Invasor = Invasor {
    posicaoInvasorX :: Float,
    posicaoInvasorY :: Float,
    velocidadeInvasor :: Float
} deriving (Show)

-- Tipo para representar o estado dos invasores
data EstadoInvasores = EstadoInvasores {
    invasores :: [Invasor],  -- Lista de invasores ativos
    direcaoInvasores :: Float,  -- Direção de movimento (-1 para esquerda, 1 para direita)
    tempoProximoTiro :: Float,  -- Tempo até o próximo tiro
    tirosInimigos :: [Tiro]  -- Lista de tiros ativos dos invasores
} deriving (Show)
