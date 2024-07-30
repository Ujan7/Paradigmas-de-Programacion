module Dibujos.Escher where

import Dibujo 
import FloatingPic (Conf(..), Output, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss (line, pictures)
import Interp()

type Escher = Int

interpEscher :: Output Escher
-- Espacio vacio
interpEscher 0 _ _ _ = pictures []
-- Triangulo
interpEscher 1 x y z = line $ map (x V.+) [zero, z, y, zero]
-- Efe
interpEscher 2 x y w = line . map (x V.+) $ [
        zero,uX, p13, p33, p33 V.+ uY , p13 V.+ uY,
        uX V.+ 4 V.* uY ,uX V.+ 5 V.* uY, x4 V.+ y5,
        x4 V.+ 6 V.* uY, 6 V.* uY, zero
    ]
    where
        p33 = 3 V.* (uX V.+ uY)
        p13 = uX V.+ 3 V.* uY
        x4 = 4 V.* uX
        y5 = 5 V.* uY
        uX = (1/6) V.* y
        uY = (1/6) V.* w
interpEscher _ _ _ _ = undefined

aFigura :: Escher -> Dibujo Escher
aFigura = figura

espejarRotar :: Dibujo Escher -> Dibujo Escher
espejarRotar p = espejar(rot45 p)

espejarRotar2 :: Dibujo Escher -> Dibujo Escher
espejarRotar2 p = r270 (espejarRotar p)

-- El dibujo u.
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU p = encimar4 (espejarRotar p)

-- El dibujo t.
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT p = p ^^^ espejarRotar p ^^^ espejarRotar2 p

-- Lado con nivel de detalle.
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 0 _ = figura 0
lado n p = cuarteto (lado (n-1) p) (lado (n-1) p) (rotar (dibujoT p)) (dibujoT p)

-- Esquina con nivel de detalle en base a la figura p.
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 0 _ = figura 0
esquina n p = cuarteto (esquina (n-1) p) (lado (n-1) p) (rotar (lado (n-1) p)) (dibujoU p)

-- Por suerte no tenemos que poner el tipo!
noneto p q r s t u v w x =
  apilar 1 2
    (juntar 1 2 p (q /// r))
    (juntar 1 2 s (t /// u) .-. juntar 1 2 v (w /// x))

-- El dibujo de Escher: 
escher :: Int -> Escher -> Dibujo Escher
escher n p =
    noneto 
        (esquina n (aFigura p))
        (lado n (aFigura p)) 
        (r270 $ esquina n (aFigura p)) 
        (rotar $ lado n (aFigura p)) 
        (dibujoU (aFigura p))
        (r270 $ lado n (aFigura p)) 
        (rotar $ esquina n (aFigura p)) 
        (r180 $ lado n (aFigura p)) 
        (r180 $ esquina n (aFigura p))


escherConf :: Conf
escherConf = Conf {
    name = "Escher",
    pic = escher 10 1,
    bas = interpEscher
}