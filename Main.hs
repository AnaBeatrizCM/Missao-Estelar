module Main where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap (loadBMP)
import LogicaJogador
import LogicaInvasores
import Controll
import Tipos

main :: IO ()
main = do
    imgNave <- loadBMP "./bmps/nave.bmp"
    imgInvasores <- loadBMP "./bmps/invasores.bmp"
    play
        (InWindow "Missao Estelar" (round larguraJanela, round alturaJanela) (100, 100))
        black
        60
        estadoInicialJogo
        (naveTiro imgNave imgInvasores)
        inputKey
        atualizarEstado
