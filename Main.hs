module Main where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap (loadBMP)  -- Importa loadBMP do módulo correto
import LogicaJogador -- Importa a lógica do jogador
import LogicaInvasores -- Importa a lógica dos invasores
import Controll       -- Importa inputKey
import Tipos          -- Importa os tipos compartilhados


main :: IO ()
main = do
    imgNave <- loadBMP "./bmps/nave.bmp"  -- Carrega a imagem da nave
    imgInvasores <- loadBMP "./bmps/invasores.bmp" -- Carrega a imagem dos invasores
    play
        (InWindow "Missao Estelar" (round larguraJanela, round alturaJanela) (100, 100))
        black
        60
        estadoInicialJogo               -- Estado inicial do jogo
        (naveTiro imgNave imgInvasores) -- Renderiza a nave e tiros
        inputKey                        -- Processa eventos do teclado
        atualizarEstado                 -- Atualiza o estado do jogo
