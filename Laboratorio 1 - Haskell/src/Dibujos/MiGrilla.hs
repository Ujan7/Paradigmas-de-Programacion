module Dibujos.MiGrilla where

import Dibujo (Dibujo, figura)
import FloatingPic(Conf(..), Output)
import Graphics.Gloss (text, scale, translate)
import Dibujos.Grilla (grilla)

type Dupla = (Int, Int)

interpDupla :: Output Dupla
interpDupla (x, y) _ _ _ = translate posX posY $ scale (1/8) (1/8) (text (show (y, x)))
{- x: Columnas, y: Filas => Intercambiamos sus posiciones en la dupla para que quede acorde a las coordenadas de la matriz -}

  where
    -- Tamaño de la pantalla
    screenSize = 800
    -- Tamaño de cada celda en la cuadrícula
    cellSize = screenSize / 8
    -- Posición X de la dupla en la pantalla
    posX = fromIntegral x * cellSize + cellSize / 2
    -- Posición Y de la dupla en la pantalla
    posY = screenSize - fromIntegral y * cellSize - cellSize / 2
    {- 
     - Como (0,0) se ubica por defecto en la parte inferior izquierda 
     - espejamos las posiciones en el eje Y para que se imprima comenzando por la parte superior izquierda 
     -}
 
figCoordenada :: Int -> Int -> Dibujo Dupla
figCoordenada x y = figura (x, y)

coordenadas :: Int -> Int -> [[Dibujo Dupla]]
coordenadas n m = [[figCoordenada i j | i <- [0..n-1]] | j <- [0..m-1]]
{- 
 - Listas por comprensión :: [ expresion | generadores, condiciones ]
 - expresion: es la expresión que se evaluará para cada combinación de valores generados.
 - generadores: son las listas de las que se tomarán los valores para evaluar la expresión.
 - condiciones: son las condiciones que deben cumplir los valores generados para ser evaluados en la expresión.
 - coordenadas : Lista de 'j' listas, donde cada lista contiene 'i' figuras de coordenadas (i, j)
 -}

testGrilla :: Dibujo Dupla
testGrilla = grilla $ coordenadas 8 8

miGrillaConf :: Conf
miGrillaConf = Conf {
    name = "MiGrilla"
    , pic = testGrilla
    , bas = interpDupla
}
