module Proyectil where
import Data.Fixed (mod')  -- Para trabajar con ángulos en radianes
import Control.Concurrent (threadDelay)  -- Para agregar pausas
import System.Process (system)  -- Para limpiar la consola

-- CONSTANTES

-- Constante de gravedad
g :: Double
g = 9.8

--Largo de la matriz principal
largoMatrix :: Int
largoMatrix = 50

-- Función que determina el ángulo según el modo del cañon
getDegree :: Char -> Int -> Int
getDegree 'r' 0 = 0
getDegree 'r' 1 = 30
getDegree 'r' 2 = 45
getDegree 'r' 3 = 60
getDegree 'l' 0 = 180
getDegree 'l' 1 = 150
getDegree 'l' 2 = 135
getDegree 'l' 3 = 120

-- Función para convertir grados a radianes
degToRad :: Double -> Double
degToRad degrees = degrees * pi / 180

--Función que determina coordenada del cañon según el tipo de barco
getDelta :: Char -> Int -> (Int, Int)
getDelta 'r' 0 = (12, 1)
getDelta 'r' 1 = (11, 0)
getDelta 'r' 2 = (11, -1)
getDelta 'r' 3 = (6, -1)
getDelta 'l' 0 = (-12, 1)
getDelta 'l' 1 = (-11, 0)
getDelta 'l' 2 = (-11, -1)
getDelta 'l' 3 = (-6, -1)

getCannonCoord :: (Int, Int) -> Char -> Int -> (Int, Int) 
getCannonCoord (x, y) h a = 
    let (dx, dy) = getDelta h a
    in ((x + dx), (largoMatrix - (y + dy) - 1))

-- Marcar la posición de la bala en la matriz
markMatrix :: [[Char]] -> (Int, Int) -> [[Char]]
markMatrix matrix (x, y) = 
  [ [ if (i, j) == (y,x) then '0' else cell | (j, cell) <- zip [0..] row ] | (i, row) <- zip [0..] matrix ]

-- Mostrar la matriz y limpiar la consola
printMatrix :: [[Char]] -> IO ()
printMatrix matrix = do
  system "clear"  -- Código ANSI para limpiar la consola
  mapM_ putStrLn matrix

-- Función que calcula la posición (x, y) de la bala en cualquier tiempo t
calculaPos :: Int -> Int -> Double -> Int -> Int -> (Int, Int)
calculaPos v0 angle t x0 y0 = (x, y)
  where
    -- Convertimos el ángulo a radianes
    theta = degToRad (fromIntegral angle)
    -- Calculamos las componentes de la posición, agregando la posición inicial (x0, y0)
    x = round $ (fromIntegral x0) + (fromIntegral v0) * cos theta * t
    y = round $ (fromIntegral y0) + (fromIntegral v0) * sin theta * t - 0.5 * g * t^2

-- Simular la trayectoria paso a paso con posición inicial (x0, y0)
simulateTrajectory :: Int -> Int -> (Int, Int) -> [[Char]] -> Double -> IO () 
simulateTrajectory v0 angle (x0,y0) matrix maxTime = mapM_ simulateStep times
  where
    times = [0, 0.1 .. maxTime]
    simulateStep t = do
      let (x, y) = calculaPos v0 angle t x0 y0
      if x >= 0 && x < length (head matrix) && y >= 0 && y < length matrix 
        then do
          let newMatrix = markMatrix matrix (x, length matrix - y - 1)
          printMatrix newMatrix
          threadDelay 100000  -- Pausa de 0.5 segundos (500,000 microsegundos)
        else return ()

dispararProyectil:: (Int, Int) -> Char -> Int -> [[Char]]-> IO()
dispararProyectil (x, y) h a matriz = simulateTrajectory 35 angle cannonCoord matriz 1000
  where
    cannonCoord = getCannonCoord (x, y) h a
    angle = getDegree h a