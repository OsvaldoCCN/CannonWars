import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode(NoBuffering))
import Text.Printf (printf)  -- Para formatear salida
import System.Process (system)  -- Para limpiar la consola
import Graphics
import Proyectil

--------------------------------------------------------------------------------------------------
-- CONSTANTES

-- Largo de la matriz principal
largoMatrix :: Int
largoMatrix = 50

loop :: (Int, Int) -> (Int, Int) -> Int -> Int -> [[Char]] -> IO ()
loop (x1, y1) (x2, y2) a1 a2 matrix = do
    let newMatrix = actualizaMatriz matrix (x1, y1) a1 'r' --dibuja barco izquierdo
    let newMatrix2 = actualizaMatriz newMatrix (x2, y2) a2 'l' --dibuja barco derecho
    system "clear"
    mapM_ putStrLn newMatrix2 --imprime la matriz
    c <- getChar
    case c of
        'a' -> do
            let newCoord = (x1 - 1, y1)  -- Mover barco izquierdo hacia la izquierda
            loop newCoord (x2, y2) a1 a2 matrix
        'd' -> do
            let newCoord = (x1 + 1, y1)  -- Mover barco izquierdo hacia la derecha
            loop newCoord (x2, y2) a1 a2 matrix
        'w' -> do
            let newA1 = if a1 < 3 then a1 + 1 else a1 -- Mover ángulo de cañón derecho hacia arriba
            loop (x1, y1) (x2, y2) newA1 a2 matrix
        's' -> do
            let newA1 = if a1 > 0 then a1 - 1 else a1 -- Mover ángulo de cañón derecho hacia abajo
            loop (x1,y1) (x2, y2) newA1 a2 matrix
        'j' -> do
            let newCoord = (x2 - 1, y2)  -- Mover barco derecho hacia la izquierda
            loop (x1, y1) newCoord a1 a2 matrix
        'l' -> do
            let newCoord = (x2 + 1, y2)  -- Mover barco derecho hacia la derecha
            loop (x1, y1) newCoord a1 a2 matrix
        'i' -> do
            let newA2 = if a2 < 3 then a2 + 1 else a2 -- Mover ángulo de cañón izquierdo hacia arriba
            loop (x1, y1) (x2, y2) a1 newA2 matrix
        'k' -> do
            let newA2 = if a2 > 0 then a2 - 1 else a2  -- Mover ángulo de cañón izquierdo hacia abajo
            loop (x1, y1) (x2, y2) a1 newA2 matrix
        'p' -> do
            dispararProyectil (x1, y1) 'r' a1 newMatrix2 -- Barco izquierdo dispara proyectil
            loop (x1, y1) (x2, y2) a1 a2 matrix
        'o' -> do
            dispararProyectil (x2, y2) 'l' a2 newMatrix2 -- Barco derecho dispara proyectil
            loop (x1, y1) (x2, y2) a1 a2 matrix
        'q' -> return ()                -- Salir del bucle
        _  -> loop (x1, y1) (x2, y2) a1 a2 matrix -- Ignorar otras teclas y repetir
---------------------------------------------------------------------------
main :: IO ()
main = do
    hSetBuffering stdin NoBuffering -- Evitar el buffering en la entrada
    hSetEcho stdin False            -- Desactivar la impresión de teclas
    putStrLn "Presiona teclas (q para salir):"
    let coordBarcoL = (20, 43)
    let coordBarcoR = (150, 43)
    let matrix = initialMatrix    -- Se invoca la matriz inicial (Graphics.hs)
    loop coordBarcoL coordBarcoR 0 0 matrix              -- Coordenadas iniciales