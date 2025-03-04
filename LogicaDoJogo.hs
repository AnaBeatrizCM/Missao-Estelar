module EstadoJogo where

import LogicaJogador (atualizarEstado, jogadorVivo, verificarColisoes)
import LogicaInvasores (atualizarInvasores)
import Tipos  -- Importa os tipos compartilhados

-- Definição do estado do jogo
data EstadoJogo = EstadoJogo {
    jogador :: EstadoJogador,
    invasores :: EstadoInvasores,
    pontuacao :: Int,
    nivel :: Int,
    jogoAtivo :: Bool
} deriving (Show)

-- Estado inicial do jogo
estadoInicial :: EstadoJogo
estadoInicial = EstadoJogo {
    jogador = estadoInicialJogador,
    invasores = estadoInicialInvasores,
    pontuacao = 0,
    nivel = 1,
    jogoAtivo = True
}

-- Atualizar pontuação e nível
atualizarPontuacao :: EstadoJogo -> Int -> EstadoJogo
atualizarPontuacao estado pontos =
    let novaPontuacao = pontuacao estado + pontos
        novoNivel = 1 + (novaPontuacao `div` 10)  -- Sobe de nível a cada 10 pontos
    in estado { pontuacao = novaPontuacao, nivel = novoNivel }

-- Perder vida e verificar fim de jogo
perderVida :: EstadoJogo -> EstadoJogo
perderVida estado
    | vidas (jogador estado) > 1 = estado { jogador = (jogador estado) { vidas = vidas (jogador estado) - 1 } }
    | otherwise = estado { jogador = (jogador estado) { vidas = 0 }, jogoAtivo = False }

-- Verificar se o jogo ainda está ativo
verificarFimJogo :: EstadoJogo -> Bool
verificarFimJogo = jogoAtivo

-- Atualizar o estado do jogo a cada frame
atualizarEstadoJogo :: Float -> EstadoJogo -> IO EstadoJogo
atualizarEstadoJogo dt estado = do
    let jogadorAtualizado = atualizarEstado dt (jogador estado)
    (invasoresAtualizados, tirosInimigos) <- atualizarInvasores dt (invasores estado)
    let jogadorFinal = verificarColisoes (nave jogadorAtualizado) tirosInimigos
        estadoComVida = if not (jogadorVivo jogadorFinal) then perderVida estado else estado
        estadoComPontuacao = atualizarPontuacao estadoComVida (length tirosInimigos)
        jogoContinua = jogoAtivo estadoComPontuacao
    return estadoComPontuacao {
        jogador = EstadoJogador jogadorFinal (tiros jogadorAtualizado),
        invasores = invasoresAtualizados,
        jogoAtivo = jogoContinua
    }
