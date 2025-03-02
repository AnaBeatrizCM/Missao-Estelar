module Main where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap (loadBMP)  -- Importa loadBMP do módulo correto
import LogicaJogador  -- Importa a lógica do jogo
import Controll       -- Importa inputKey
import Tipos          -- Importa os tipos compartilhados

main :: IO ()
main = do
    imgNave <- loadBMP "./nave.bmp"  -- Carrega a imagem da nave
    play
        (InWindow "Teste" (round larguraJanela, round alturaJanela) (100, 100))
        black
        60
        (EstadoJogador naveInicial [])  -- Estado inicial do jogo
        (naveTiro imgNave)             -- Renderiza a nave e tiros
        inputKey                        -- Processa eventos do teclado
        atualizarEstado                 -- Atualiza o estado do jogo
