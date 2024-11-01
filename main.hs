import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode(NoBuffering))
import Text.Printf (printf)  -- Para formatear salida
import System.Process (system)  -- Para limpiar la consola
import Graphics
import Proyectil
import Tools

--------------------------------------------------------------------------------------------------
-- CONSTANTES

-- Largo de la matriz principal
largoMatrix :: Int
largoMatrix = 50

loop :: (Canon (Int, Int, Int, Int, Int)) -> (Canon (Int, Int, Int, Int, Int)) -> [[Char]] -> IO ()
loop canon1 canon2 matrix = do
    let newMatrix = actualizaMatriz matrix (getX canon1, getY canon1) (getAngle canon1) 'r' --dibuja barco izquierdo
    let newMatrix2 = actualizaMatriz newMatrix (getX canon2, getY canon2) (getAngle canon2) 'l' --dibuja barco derecho
    printMatrix newMatrix2 --imprime la matriz
    putStrLn (show $ getLife canon1)
    putStrLn (show $ getLife canon2)
    c <- getChar
    case c of
        'a' -> do
            let newCanon = if (getX canon1) > 12 then  canon1 >>= (moveX (-1)) else canon1 -- Mover barco izquierdo hacia la izquierda
            loop newCanon canon2 matrix
        'd' -> do
            let newCanon = if (getX canon1) < 59 then  canon1 >>= (moveX 1) else canon1 -- Mover barco izquierdo hacia la derecha
            loop newCanon canon2 matrix
        'w' -> do
            let newCanon = if (getAngle canon1) < 3 then  canon1 >>= (moveAngle 1) else canon1 -- Mover ángulo de cañón derecho hacia arriba
            loop  newCanon canon2 matrix
        's' -> do
            let newCanon = if (getAngle canon1) > 0 then  canon1 >>= (moveAngle (-1)) else canon1 -- Mover ángulo de cañón derecho hacia abajo
            loop newCanon canon2 matrix
        'j' -> do
            let newCanon = if (getX canon2) > 116 then  canon2 >>= (moveX (-1)) else canon2 -- Mover barco derecho hacia la izquierda
            loop canon1 newCanon matrix
        'l' -> do
            let newCanon = if (getX canon2) < 156 then  canon2 >>= (moveX 1) else canon2 -- Mover barco derecho hacia la derecha
            loop canon1 newCanon matrix
        'i' -> do
            let newCanon = if (getAngle canon2) < 3 then canon2 >>= (moveAngle 1) else canon2 -- Mover ángulo de cañón izquierdo hacia arriba
            loop canon1 newCanon matrix
        'k' -> do
            let newCanon = if (getAngle canon2) > 0 then canon2 >>= (moveAngle (-1)) else canon2 -- Mover ángulo de cañón izquierdo hacia abajo
            loop canon1 newCanon matrix
        'p' -> do
            newCanon <- dispararProyectil canon1 canon2 'r' newMatrix2 -- Barco izquierdo dispara proyectil
            loop canon1 newCanon matrix
        'o' -> do
            newCanon <- dispararProyectil canon2 canon1 'l' newMatrix2 -- Barco derecho dispara proyectil
            loop newCanon canon2 matrix
        'q' -> return ()                -- Salir del bucle
        _  -> loop canon1 canon2 matrix -- Ignorar otras teclas y repetir
---------------------------------------------------------------------------
main :: IO ()
main = do
    hSetBuffering stdin NoBuffering -- Evitar el buffering en la entrada
    hSetEcho stdin False            -- Desactivar la impresión de teclas
    putStrLn "Presiona teclas (q para salir):"
    let canon1 = Canon (30, 100, 20, 43, 0)
    let canon2 = Canon (30, 100, 150, 43, 0)
    let matrix = initialMatrix    -- Se invoca la matriz inicial (Graphics.hs)
    loop canon1 canon2 matrix              -- Coordenadas iniciales