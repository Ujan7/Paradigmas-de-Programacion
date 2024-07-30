module Dibujos.Euge where 

import Dibujo 
import FloatingPic (Conf(..), Output, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss (line, pictures)
import Interp()

type Euge = Int

interpEuge :: Output Euge
-- Espacio vacio
interpEuge 0 _ _ _ = pictures []
-- Triangulo
interpEuge 1 x y z = line $ map (x V.+) [zero, z, y, zero]
interpEuge _ _ _ _ = undefined

euge = rotar (figura 1)

eugeConf :: Conf
eugeConf = Conf {
    name = "euge",
    pic = euge,
    bas = interpEuge
}