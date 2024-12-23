module Proyectil where
import Data.Fixed (mod')  -- Para trabajar con ángulos en radianes
import Control.Concurrent (threadDelay)  -- Para agregar pausas
import System.Process (system)  -- Para limpiar la consola
import Tools
import System.Random

-- CONSTANTES

-- Constante de gravedad
g :: Double
g = 9.8

--Alto de la matriz principal
altoMatrix :: Int
altoMatrix = 50

--Largo de la matriz principal
largoMatrix :: Int
largoMatrix = 169


--Obtener Rango de hitbox
getHitBox :: (Int, Int) -> ((Int, Int), (Int, Int))
getHitBox (x, y) = ((x-11, x+11), (altoMatrix - y - 2, altoMatrix - y))

--Función que determina si un barco fue impactado o no
fueImpactado :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Bool
fueImpactado (x, y) ((lim1, lim2), (lim3, lim4))
  | (x >= lim1 && x <= lim2) && (y >= lim3 && y <= lim4) = True
  | otherwise = False

impactaIceberg :: (Int, Int) -> Bool
impactaIceberg (x,y)
  | x >= 76 && x <= 104 && y < 9 = True
  | x >= 79 && x <= 99 && y >= 9 && y <= 16 = True
  | otherwise = False

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
    in ((x + dx), (altoMatrix - (y + dy) - 1))

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
simulateTrajectory :: Canon (Int, Int, Int, Int, Int) -> Int -> Int -> (Int, Int) -> [[Char]] -> Double -> StdGen -> IO (Canon (Int, Int, Int, Int, Int))
simulateTrajectory canon2 v0 degree (x0, y0) matrix maxTime gen = simulateStep canon2 0
  where
    simulateStep canon2 t
      | t > maxTime = return canon2  -- Termina la simulación y devuelve el estado final del cañón
      | otherwise = do
          let (x, y) = calculaPos v0 degree t x0 y0
          if x >= 0 && x < largoMatrix && y >= 0 && y < length matrix && not (impactaIceberg (x,y))
            then do
              if (fueImpactado (x,y) (getHitBox (getX canon2, getY canon2))) then do 
                                let newCanon = canon2 >>= impactar gen
                                return newCanon
              else do
                let newMatrix = markMatrix matrix (x, length matrix - y - 1)
                printMatrix newMatrix
                threadDelay 100000  -- Pausa de 0.5 segundos (100,000 microsegundos)
                simulateStep canon2 (t + 0.1)  -- Llama recursivamente con el nuevo estado del cañón y el tiempo incrementado
            else do 
              if (y >= 0 && not (impactaIceberg (x,y))) then simulateStep canon2 (t + 0.1) else return canon2  -- Si la posición de la bala sale de los límites, termina la simulación y devuelve el cañón


dispararProyectil:: Canon (Int, Int, Int, Int, Int) -> Canon (Int, Int, Int, Int, Int)  -> Char -> [[Char]]-> StdGen -> IO (Canon(Int, Int, Int, Int, Int))
dispararProyectil canon1 canon2 h matriz gen = simulateTrajectory canon2 35 angle cannonCoord matriz 50 gen
  where
    cannonCoord = getCannonCoord (getX canon1, getY canon1) h (getAngle canon1)
    angle = (getDegree h (getAngle canon1)) +  (fst (randomR (-5,5) gen :: (Int, StdGen)))