module LogicaInvasores where

import System.Random (randomRIO)

-- Definindo constantes da tela
larguraJanela :: Float
larguraJanela = 800

alturaJanela :: Float
alturaJanela = 600

-- Definindo o tipo Tiro
data Tiro = Tiro {
    posicaoTiroX :: Float,
    posicaoTiroY :: Float,
    velocidadeTiro :: Float
} deriving (Show)

-- Definindo o tipo Invasor
data Invasor = Invasor {
    posicaoInvasorX :: Float,
    posicaoInvasorY :: Float,
    velocidadeInvasor :: Float
} deriving (Show)

-- Tipo para gerenciar o estado dos invasores
data EstadoInvasores = EstadoInvasores {
    invasores :: [Invasor],  -- Lista de invasores ativos
    direcaoInvasores :: Float,  -- Direção de movimento (-1 para esquerda, 1 para direita)
    tempoProximoTiro :: Float,  -- Tempo até o próximo tiro
    tirosInimigos :: [Tiro]  -- Lista de tiros ativos dos invasores
} deriving (Show)

-- Estado inicial dos invasores
estadoInicialInvasores :: EstadoInvasores
estadoInicialInvasores = EstadoInvasores {
    invasores = [Invasor x y 2 | x <- [100, 200, 300, 400, 500], y <- [500, 450, 400]],  -- Posições iniciais
    direcaoInvasores = 1,  -- Começam se movendo para a direita
    tempoProximoTiro = 2,  -- Tempo inicial para o próximo tiro
    tirosInimigos = []  -- Sem tiros no início
}

-- Movimentação dos invasores
moverInvasores :: EstadoInvasores -> EstadoInvasores
moverInvasores estado@(EstadoInvasores invs dir tempo tiros) =
    let novosInvasores = map (\inv -> inv { posicaoInvasorX = posicaoInvasorX inv + velocidadeInvasor inv * dir }) invs
        (novosInvasoresAjustados, novaDirecao) = ajustarMovimento novosInvasores dir
    in estado { invasores = novosInvasoresAjustados, direcaoInvasores = novaDirecao }

-- Ajusta o movimento dos invasores quando atingem os limites da tela
ajustarMovimento :: [Invasor] -> Float -> ([Invasor], Float)
ajustarMovimento invs dir
    | any (\inv -> posicaoInvasorX inv <= 0 || posicaoInvasorX inv >= larguraJanela) invs =
        (map (\inv -> inv { posicaoInvasorY = posicaoInvasorY inv - 20 }) invs, -dir)  -- Desce e inverte a direção
    | otherwise = (invs, dir)

-- Disparo dos invasores
dispararInvasores :: EstadoInvasores -> IO ([Tiro], EstadoInvasores)
dispararInvasores estado@(EstadoInvasores invs _ tempo tiros) = do
    if tempo <= 0
        then do
            invasorAleatorio <- escolherInvasorAleatorio invs
            let novoTiro = Tiro (posicaoInvasorX invasorAleatorio) (posicaoInvasorY invasorAleatorio) 5  -- Tiro com velocidade 5
            return ([novoTiro], estado { tempoProximoTiro = 2, tirosInimigos = novoTiro : tiros })  -- Reseta o tempo e adiciona o tiro
        else return ([], estado { tempoProximoTiro = tempo - 0.1 })  -- Decrementa o tempo

-- Escolhe um invasor aleatório para disparar
escolherInvasorAleatorio :: [Invasor] -> IO Invasor
escolherInvasorAleatorio invs = do
    indice <- randomRIO (0, length invs - 1)
    return (invs !! indice)

-- Verifica colisões entre tiros do jogador e invasores
verificarColisoesTirosInvasores :: [Tiro] -> [Invasor] -> ([Tiro], [Invasor])
verificarColisoesTirosInvasores tiros invs =
    let tirosRestantes = filter (\tiro -> not (any (colideComTiroInvasor tiro) invs)) tiros
        invasoresRestantes = filter (\inv -> not (any (colideComTiroInvasor inv) tiros)) invs
    in (tirosRestantes, invasoresRestantes)

-- Verifica se um tiro colide com um invasor
colideComTiroInvasor :: Tiro -> Invasor -> Bool
colideComTiroInvasor (Tiro tx ty _) (Invasor ix iy _) =
    abs(tx - ix) < 20 && abs(ty - iy) < 20  -- Raio de colisão aproximado

-- Atualiza o estado dos invasores
atualizarInvasores :: Float -> EstadoInvasores -> IO (EstadoInvasores, [Tiro])
atualizarInvasores dt estado = do
    let estadoMovido = moverInvasores estado
    (tirosInimigos, estadoAtualizado) <- dispararInvasores estadoMovido
    return (estadoAtualizado, tirosInimigos)