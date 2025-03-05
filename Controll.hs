module Controll (inputKey) where

import Graphics.Gloss.Interface.Pure.Game  -- Importa Event para capturar teclas
import Tipos  -- Importa os tipos compartilhados
import LogicaJogador (moverNave)  -- Importa moverNave

-- Captura eventos do teclado para movimentação da nave
inputKey :: Event -> EstadoJogador -> EstadoJogador
inputKey (EventKey (Char 'a') Down _ _) (EstadoJogador nave tiros invasores pontuacao) =
    EstadoJogador (moverNave nave (-3)) tiros invasores pontuacao -- Move a nave para a esquerda
inputKey (EventKey (Char 'd') Down _ _) (EstadoJogador nave tiros invasores pontuacao) =
    EstadoJogador (moverNave nave 3) tiros invasores pontuacao -- Move a nave para a direita
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (EstadoJogador nave tiros invasores pontuacao) =
    EstadoJogador nave (novoTiro : tiros) invasores pontuacao -- Cria um novo tiro
  where
    novoTiro = Tiro (posicaoX nave) (posicaoY nave + raioNave-10) 7  -- Tiro sai da nave e sobe
inputKey _ estado = estado  -- Ignora outras teclas
