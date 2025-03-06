module Controll (inputKey) where

import Graphics.Gloss.Interface.Pure.Game
import Tipos
import LogicaJogador (moverNave)

inputKey :: Event -> EstadoJogador -> EstadoJogador
inputKey (EventKey (Char 'a') Down _ _) (EstadoJogador nave tiros invasores pontuacao) =
    EstadoJogador (moverNave nave (-3)) tiros invasores pontuacao
inputKey (EventKey (Char 'd') Down _ _) (EstadoJogador nave tiros invasores pontuacao) =
    EstadoJogador (moverNave nave 3) tiros invasores pontuacao
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (EstadoJogador nave tiros invasores pontuacao) =
    EstadoJogador nave (novoTiro : tiros) invasores pontuacao
  where
    novoTiro = Tiro (posicaoX nave) (posicaoY nave + raioNave-10) 7
inputKey _ estado = estado
