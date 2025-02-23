module Controll where
    
import LogicaJogador

-- Captura as teclas para movimentação da nave 

inputKey :: EstadoJogador -> Char -> EstadoJogador

inputKey (EstadoJogador nave tiros) char

    -- Usando a recepção da tecla 'a' para mover para esquerda

    | char == 'a' = EstadoJogador (moverNave nave (-3)) tiros

    -- USando a recepção da tecla 'd' para mover a direita 

    | char == 'd' = EstadoJogador (moverNave nave 3) tiros

    -- Usando BackSapce para lançar tiros 

    | char == ' ' = EstadoJogador nave(novoTiro : tiros) 

    -- Caso as teclas tratadas não sejam tecladas nada acontece 

    | otherwise = EstadoJogador nave tiros

    -- Criando tiro para armazenar como ativo

    where
        novoTiro = Tiro (posicaoX nave) 550 7