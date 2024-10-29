import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode(NoBuffering))
import Data.Fixed (mod')  -- Para trabajar con ángulos en radianes
import Control.Concurrent (threadDelay)  -- Para agregar pausas
import Text.Printf (printf)  -- Para formatear salida
import System.Process (system)  -- Para limpiar la consola


-- ------------------------------------------------------------------------------------------------



getDegree :: Int -> Int
getDegree 0 = 0
getDegree 1 = 30
getDegree 2 = 45
getDegree 3 = 60


getCannonCoord :: (Int, Int)-> Int -> Char -> Int -> (Double, Double) 
getCannonCoord (x, y) largo h a = 
    let (dx, dy) = getDelta h a
    in ((fromIntegral $ x + dx),(fromIntegral $ largo -(y + dy) -1))

getDelta :: Char -> Int -> (Int, Int)
getDelta 'r' 0 = (12, 1)
getDelta 'r' 1 = (11, 0)
getDelta 'r' 2 = (11, -1)
getDelta 'r' 3 = (6, -1)
getDelta 'l' 0 = (-12, 1)
getDelta 'l' 1 = (-11, 0)
getDelta 'l' 2 = (-11, -1)
getDelta 'l' 3 = (-6, -1)

wrapperCannon:: (Int, Int) -> Int -> Char -> Int -> [[Char]]-> IO()
wrapperCannon (x, y) largo h a matriz = simulateTrajectory 35 angle cannonCoord matriz 1000
  where
    cannonCoord = getCannonCoord (x, y) largo h a
    angle = (fromIntegral $ getDegree a)

-- Constante de gravedad
g :: Double
g = 9.8

-- Función para convertir grados a radianes
degToRad :: Double -> Double
degToRad degrees = degrees * pi / 180

-- Función que calcula la posición (x, y) de la bala en cualquier tiempo t
cannonPosition :: Double -> Double -> Double -> Double -> Double -> (Double, Double)
cannonPosition v0 angle t x0 y0 = (x, y)
  where
    -- Convertimos el ángulo a radianes
    theta = degToRad angle
    -- Calculamos las componentes de la posición, agregando la posición inicial (x0, y0)
    x = x0 + v0 * cos theta * t
    y = y0 + v0 * sin theta * t - 0.5 * g * t^2

-- Crear una matriz vacía de tamaño dado
createMatrix :: Int -> Int -> [[Char]]
createMatrix rows cols = replicate rows (replicate cols '.')

-- Marcar la posición de la bala en la matriz
markMatrix :: [[Char]] -> (Int, Int) -> [[Char]]
markMatrix matrix (x, y) = 
  [ [ if (i, j) == (y,x) then '0' else cell | (j, cell) <- zip [0..] row ] | (i, row) <- zip [0..] matrix ]

-- Mostrar la matriz y limpiar la consola
printMatrix :: [[Char]] -> IO ()
printMatrix matrix = do
  system "clear"  -- Código ANSI para limpiar la consola
  mapM_ putStrLn matrix

-- Simular la trayectoria paso a paso con posición inicial (x0, y0)
simulateTrajectory :: Double -> Double -> (Double, Double) -> [[Char]] -> Double -> IO () 
simulateTrajectory v0 angle (x0,y0) matrix maxTime = mapM_ simulateStep times
  where
    times = [0, 0.1 .. maxTime]
    simulateStep t = do
      let (x, y) = cannonPosition v0 angle t x0 y0
          xInt = round x
          yInt = round y
      if xInt >= 0 && xInt < length (head matrix) && yInt >= 0 && yInt < length matrix 
        then do
          let newMatrix = markMatrix matrix (xInt, length matrix - yInt - 1)
          printMatrix newMatrix
          threadDelay 100000  -- Pausa de 0.5 segundos (500,000 microsegundos)
        else return ()
-- -------------------------------------------------------------------------
main :: IO ()
main = do
    hSetBuffering stdin NoBuffering -- Evitar el buffering en la entrada
    hSetEcho stdin False            -- Desactivar la impresión de teclas
    putStrLn "Presiona teclas (q para salir):"
    let coord1 = (20, 43)
    let coord2 = (150, 43)
    let matrix = 
          [ "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                                                                                                       |"
          , "|                                                                                     ▒▒▒▒▒                                                                             |"
          , "|                                                                                   ▒▒▒▒▒▒▒▒▒▒▒                                                                         |"
          , "|                                                                                 ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                       |"
          , "|                                                                               ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                      |"
          , "|                                                                               ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                    |"
          , "|                                                                              ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                   |"
          , "|                                                                              ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                  |"
          , "|                                                                           ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                 |"
          , "|                                                                           ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                 |"
          , "|                                                                          ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                 |"
          , "|                                                                          ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                 |"
          , "|                                                                        ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                               |"
          , "|                                                                      ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                               |"
          , "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"
          , "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"
          , "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"
          ]
    loop coord1 coord2 0 0 matrix              -- Coordenadas iniciales

replaceSubMatrix :: [[Char]] -> (Int, Int) -> Int -> Char -> [[Char]]
replaceSubMatrix bigMatrix (x, y) a h = 
    zipWith replaceRow [0..] bigMatrix
  where
    -- Limitamos `a` para que esté entre 0 y 3
    limitedA = max 0 (min 3 a)

    -- Seleccionamos la matriz pequeña según la orientación `h` y el ángulo `limitedA`
    smallMatrix = case (h, limitedA) of
        -- Orientación derecha, ángulos 0, 30, 45, 75
        ('r', 0) -> 
          [ "                       "
          , "    █ █ █      ▄▄▄▄▄▄▄▄"
          , "┌┬┬████████  ▄██▀▀▀▀▀▀ "
          , "▀████████████████▀     "
          ]
        ('r', 1) -> 
          [ "                    ▄▄ "
          , "    █ █ █     ▄▄▄███▀▀ "
          , "┌┬┬████████  ███▀▀     "
          , "▀████████████████▀     "
          ]
        ('r', 2) -> 
          [ "                 ▄██▀ "
          , "    █ █ █      ▄██▀   "
          , "┌┬┬████████  ▄██▀     "
          , "▀████████████████▀    "
          ]
        ('r', 3) -> 
          [ "                ██    "
          , "    █ █ █      ██     "
          , "┌┬┬████████  ▄██▄     "
          , "▀████████████████▀    "
          ]
        -- Orientación izquierda, ángulos 0, 30, 45, 75
        ('l', 0) -> 
          [ "                       "
          , "▄▄▄▄▄▄▄▄      █ █ █    "
          , " ▀▀▀▀▀▀██▄  ████████┬┬┬"
          , "     ▀████████████████▀"
          ]
        ('l', 1) -> 
          [ " ▄▄                    "
          , " ▀▀███▄▄▄     █ █ █    "
          , "     ▀▀██   ████████┬┬┬"
          , "     ▀████████████████▀"
          ]
        ('l', 2) -> 
          [ "  ▀██▄                 "
          , "    ▀██▄      █ █ █    "
          , "      ▀██▄  ████████┬┬┬"
          , "     ▀████████████████▀"
          ]
        ('l', 3) -> 
          [ "     ██                "
          , "      ██      █ █ █    "
          , "      ▄██▄  ████████┬┬┬"
          , "     ▀████████████████▀"
          ]
        _ -> []  -- Manejar otros casos

    smallRows = length smallMatrix
    smallCols = length (head smallMatrix)

    -- Ajustar la coordenada para centrar smallMatrix en (x, y)
    adjustedY = y - (smallRows `div` 2)
    adjustedX = x - (smallCols `div` 2)

    replaceRow i bigRow
      | i >= adjustedY && i < adjustedY + smallRows = 
          let smallRow = smallMatrix !! (i - adjustedY)
          in replaceCols bigRow smallRow adjustedX
      | otherwise = bigRow
    
    replaceCols bigRow smallRow startCol =
        take startCol bigRow ++ smallRow ++ drop (startCol + smallCols) bigRow



loop :: (Int, Int) -> (Int, Int) -> Int -> Int -> [[Char]] -> IO ()
loop (x1, y1) (x2, y2) a1 a2 matrix = do
    let newMatrix = replaceSubMatrix matrix (x1, y1) a1 'r'
    let newMatrix2 = replaceSubMatrix newMatrix (x2, y2) a2 'l'
    system "clear"
    mapM_ putStrLn newMatrix2
    c <- getChar
    
    case c of
        'a' -> do
            let newCoord = (x1 - 1, y1)  -- Moverse hacia la izquierda
            loop newCoord (x2, y2) a1 a2 matrix
        'd' -> do
            let newCoord = (x1 + 1, y1)  -- Moverse hacia la derecha
            loop newCoord (x2, y2) a1 a2 matrix
        'w' -> do
            let newA1 = if a1 < 3 then a1 + 1 else a1
            loop (x1, y1) (x2, y2) newA1 a2 matrix
        's' -> do
            let newA1 = if a1 > 0 then a1 - 1 else a1  -- Moverse hacia abajo
            loop (x1,y1) (x2, y2) newA1 a2 matrix
        'j' -> do
            let newCoord = (x2 - 1, y2)  -- Mover la segunda matriz hacia la izquierda
            loop (x1, y1) newCoord a1 a2 matrix
        'l' -> do
            let newCoord = (x2 + 1, y2)  -- Mover la segunda matriz hacia la derecha
            loop (x1, y1) newCoord a1 a2 matrix
        'i' -> do
            let newA2 = if a2 < 3 then a2 + 1 else a2 -- Mover la segunda matriz hacia arriba
            loop (x1, y1) (x2, y2) a1 newA2 matrix
        'k' -> do
            let newA2 = if a2 > 0 then a2 - 1 else a2  -- Mover la segunda matriz hacia abajo
            loop (x1, y1) (x2, y2) a1 newA2 matrix
        'p' -> do
            wrapperCannon (x1, y1) 50 'r' a1 newMatrix2
            loop (x1, y1) (x2, y2) a1 a2 matrix

     
        'q' -> return ()                -- Salir del bucle
        _  -> loop (x1, y1) (x2, y2) a1 a2 matrix -- Ignorar otras teclas y repetir


--
--                 
--     █ █ █      ▄▄▄▄▄▄▄▄
-- ┌┬┬████████  ▄██▀▀▀▀▀▀ 
-- ▀████████████████▀     

--                  
--                     ▄▄
--     █ █ █     ▄▄▄███▀▀
-- ┌┬┬████████  ███▀▀
-- ▀████████████████▀    
         
--                   
--                  ▄██▀
--     █ █ █      ▄██▀
-- ┌┬┬████████  ▄██▀
-- ▀████████████████▀

--                  ██
--                 ██
--     █ █ █      ██
-- ┌┬┬████████  ▄██▄
-- ▀████████████████▀

--               ██
--               ██
--     █ █ █     ██
-- ┌┬┬████████  ▄██▄
-- ▀████████████████▀

--            ██
--             ██
--     █ █ █    ██
-- ┌┬┬████████  ▄██▄
-- ▀████████████████▀
