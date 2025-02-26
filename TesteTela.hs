import Gaphics.Gloss
import Gaphics.Gloss.Juicy

--Tela
largura, altura :: Int
largura = 800
altura = 600

--Inicio
type Estado = Float
estadoInicial :: Estado
estadoInicial = 0

--Atualizar frames
atualizar :: Float -> Estado -> Estado
atualizar _ x = x 

