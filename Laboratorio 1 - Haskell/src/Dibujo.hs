module Dibujo (encimar, apilar, juntar, rot45, rotar, 
                espejar, (^^^), (.-.), (///), r90, r180, 
                r270, encimar4, cuarteto, ciclar,  mapDib, 
                change, foldDib, comp, figura, Dibujo(..)
    ) where


-- nuestro lenguaje 
data Dibujo a = Figura a
              | Encimar (Dibujo a) (Dibujo a)
              | Apilar Float Float (Dibujo a) (Dibujo a)
              | Rotar (Dibujo a)
              | Espejar (Dibujo a)
              | Rot45 (Dibujo a)
              | Juntar Float Float (Dibujo a) (Dibujo a)
            deriving (Show, Eq)

-- combinadores
infixr 6 ^^^

infixr 7 .-.

infixr 8 ///

comp :: Int -> (a -> a) -> a -> a
comp 0 _ x = x
comp n f x = f (comp (n-1) f x) 


-- Funciones constructoras
figura :: a -> Dibujo a
figura = Figura

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar

juntar  :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar

rot45 :: Dibujo a -> Dibujo a
rot45 = Rot45

rotar :: Dibujo a -> Dibujo a
rotar = Rotar

espejar :: Dibujo a -> Dibujo a
espejar = Espejar

(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = Encimar 

(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = Apilar 1 1 

(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = Juntar 1 1

-- rotaciones
r90 :: Dibujo a -> Dibujo a
r90 = rotar

r180 :: Dibujo a -> Dibujo a
r180 = comp 2 rotar

r270 :: Dibujo a -> Dibujo a
r270 = comp 3 rotar


-- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 d = d ^^^ r90 d ^^^ r180 d ^^^ r270 d

-- cuatro figuras en un cuadrante.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto d1 d2 d3 d4 = (d1 /// d2) .-. (d3 /// d4)

-- un cuarteto donde se repite la imagen, rotada (¡No confundir con encimar4!)
ciclar :: Dibujo a -> Dibujo a
ciclar d = cuarteto d (r90 d) (r180 d) (r270 d)

-- map para nuestro lenguaje
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Figura x) = Figura (f x)
mapDib f (Rotar d1) = Rotar (mapDib f d1)
mapDib f (Espejar d1) = Espejar (mapDib f d1)
mapDib f (Rot45 d1) = Rot45 (mapDib f d1)
mapDib f (Apilar f1 f2 d1 d2) = Apilar f1 f2 (mapDib f d1) (mapDib f d2)
mapDib f (Juntar f1 f2 d1 d2) = Juntar f1 f2 (mapDib f d1) (mapDib f d2)
mapDib f (Encimar d1 d2) = Encimar (mapDib f d1) (mapDib f d2)
-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f

-- Cambiar todas las básicas de acuerdo a la función.
change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
change f (Figura x) = f x
change f (Rotar d1) = Rotar (change f d1)
change f (Espejar d1) = Espejar (change f d1)
change f (Rot45 d1) = Rot45 (change f d1)
change f (Apilar f1 f2 d1 d2) = Apilar f1 f2 (change f d1) (change f d2)
change f (Juntar f1 f2 d1 d2) = Juntar f1 f2 (change f d1) (change f d2)
change f (Encimar d1 d2) = Encimar (change f d1) (change f d2)

-- Principio de recursión para Dibujos.
foldDib ::
  (a -> b) ->
  (b -> b) ->
  (b -> b) ->
  (b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (b -> b -> b) ->
  Dibujo a ->
  b
foldDib f _ _ _ _ _ _ (Figura a) = f a
foldDib f g h i j k l (Rotar d1) = g (foldDib f g h i j k l d1)
foldDib f g h i j k l (Espejar d1) = h (foldDib f g h i j k l d1)
foldDib f g h i j k l (Rot45 d1) = i (foldDib f g h i j k l d1)
foldDib f g h i j k l (Apilar f1 f2 d1 d2) = j f1 f2 (foldDib f g h i j k l d1) (foldDib f g h i j k l d2)
foldDib f g h i j k l (Juntar f1 f2 d1 d2) = k f1 f2 (foldDib f g h i j k l d1) (foldDib f g h i j k l d2)
foldDib f g h i j k l (Encimar d1 d2) = l (foldDib f g h i j k l d1) (foldDib f g h i j k l d2)
 