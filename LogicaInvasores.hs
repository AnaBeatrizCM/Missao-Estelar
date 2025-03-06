module LogicaInvasores (
    estadoInicialInvasores, moverInvasores, dispararInvasores, verificarColisoesTirosInvasores, atualizarInvasores
) where

import System.Random (randomRIO)
import Tipos
import Debug.Trace

estadoInicialInvasores :: EstadoInvasores
estadoInicialInvasores = EstadoInvasores {
    invasores = [Invasor x y 2 | x <- [-150, -75, 0, 75, 150], y <- [200, 150, 100]],
    direcaoInvasores = 1,
    tempoProximoTiro = 2,
    tirosInimigos = []
}


moverInvasores :: Float -> EstadoInvasores -> EstadoInvasores
moverInvasores dt estado@(EstadoInvasores invs dir tempo tiros) =
    let novosInvasores = map (\inv -> inv { posicaoInvasorX = posicaoInvasorX inv + velocidadeInvasor inv * dir * dt * 50 }) invs
        (novosInvasoresAjustados, novaDirecao) = ajustarMovimento dt novosInvasores dir
    in estado { invasores = novosInvasoresAjustados, direcaoInvasores = novaDirecao }

ajustarMovimento :: Float -> [Invasor] -> Float -> ([Invasor], Float)
ajustarMovimento dt invs dir
    | any (\inv -> posicaoInvasorX inv <= (-larguraJanela/2) || posicaoInvasorX inv >= (larguraJanela/2)) invs =
        let novosInvasores = map (\inv -> inv { posicaoInvasorY = posicaoInvasorY inv - 10 * dt }) invs  -- Desce 10 unidades por segundo
            -- Verifica se os invasores já estão muito baixo na tela
            invasoresAjustados = if any (\inv -> posicaoInvasorY inv <= (-alturaJanela/2 + 50)) novosInvasores
                                then invs  -- Mantém os invasores na posição atual (não desce mais)
                                else novosInvasores
        in (invasoresAjustados, -dir)  -- Inverte a direção horizontal
    | otherwise = (invs, dir)


dispararInvasores :: EstadoInvasores -> IO ([Tiro], EstadoInvasores)
dispararInvasores estado@(EstadoInvasores invs _ tempo tiros) = do
    if tempo <= 0
        then do
            invasorAleatorio <- escolherInvasorAleatorio invs
            let novoTiro = Tiro (posicaoInvasorX invasorAleatorio) (posicaoInvasorY invasorAleatorio) 5
            return ([novoTiro], estado { tempoProximoTiro = 2, tirosInimigos = novoTiro : tiros })
        else return ([], estado { tempoProximoTiro = tempo - 0.1 })

escolherInvasorAleatorio :: [Invasor] -> IO Invasor
escolherInvasorAleatorio invs = do
    indice <- randomRIO (0, length invs - 1)
    return (invs !! indice)

verificarColisoesTirosInvasores :: [Tiro] -> [Invasor] -> ([Tiro], [Invasor])
verificarColisoesTirosInvasores tiros invs =
    let tirosRestantes = filter (\tiro -> not (any (colideComTiroInvasor tiro) invs)) tiros
        invasoresRestantes = filter (\inv -> not (any (\tiro -> colideComTiroInvasor tiro inv) tiros)) invs
    in (tirosRestantes, invasoresRestantes)


colideComTiroInvasor :: Tiro -> Invasor -> Bool
colideComTiroInvasor (Tiro tx ty _) (Invasor ix iy _) =
    abs(tx - ix) < 20 && abs(ty - iy) < 20

atualizarInvasores :: Float -> EstadoInvasores -> IO (EstadoInvasores, [Tiro])
atualizarInvasores dt estado = do
    let estadoMovido = moverInvasores dt estado
    (tirosInimigos, estadoAtualizado) <- dispararInvasores estadoMovido
    return (estadoAtualizado, tirosInimigos)
