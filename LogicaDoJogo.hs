module EstadoJogo where

-- Definição do estado do jogo
data EstadoJogo = EstadoJogo {
    vidas :: Int,
    pontuacao :: Int,
    nivel :: Int,
    jogoAtivo :: Bool
} deriving (Show)

-- Estado inicial do jogo
estadoInicial :: EstadoJogo
estadoInicial = EstadoJogo {
    vidas = 3,
    pontuacao = 0,
    nivel = 1,
    jogoAtivo = True
}

-- Atualizar pontuação e nível
atualizarPontuacao :: EstadoJogo -> Int -> EstadoJogo
atualizarPontuacao estado pontos =
    let novaPontuacao = pontuacao estado + pontos
        novoNivel = 1 + (novaPontuacao `div` 10)  -- Exemplo: sobe de nível a cada 10 pontos
    in estado { pontuacao = novaPontuacao, nivel = novoNivel }

-- Perder vida e verificar fim de jogo
perderVida :: EstadoJogo -> EstadoJogo
perderVida estado
    | vidas estado > 1 = estado { vidas = vidas estado - 1 }
    | otherwise = estado { vidas = 0, jogoAtivo = False }  -- Fim de jogo

-- Verificar se o jogo ainda está ativo
verificarFimJogo :: EstadoJogo -> Bool
verificarFimJogo = jogoAtivo

-- Atualizar o estado do jogo a cada frame
atualizarEstado :: EstadoJogo -> Int -> Bool -> EstadoJogo
atualizarEstado estado pontos sofreuDano =
    let estadoPontuacao = atualizarPontuacao estado pontos
        estadoFinal = if sofreuDano then perderVida estadoPontuacao else estadoPontuacao
    in estadoFinal
