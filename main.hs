import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode(NoBuffering))
import Text.Printf (printf)  -- Para formatear salida
import System.Process (system)  -- Para limpiar la consola
import Graphics
import Proyectil
import Tools
import System.Random
import Data.IORef

--------------------------------------------------------------------------------------------------
-- CONSTANTES

-- Largo de la matriz principal
largoMatrix :: Int
largoMatrix = 50

loop :: (Canon (Int, Int, Int, Int, Int)) -> (Canon (Int, Int, Int, Int, Int)) -> [[Char]] -> StdGen -> Int -> IO ()
loop canon1 canon2 matrix gen 0 = do
    system "clear" -- Limpia la consola
    let smallMatrix1 = tipoBarco 'r' (getAngle canon1)
    let smallMatrix2 = tipoBarco 'l' (getAngle canon2)
    let smallMatrix3 = mostrarDatos canon1 'r'
    let smallMatrix4 = mostrarDatos canon2 'l'
    let newMatrix1 = actualizaMatriz matrix smallMatrix1 ((getX canon1),(getY canon1))
    let newMatrix2 = actualizaMatriz newMatrix1 smallMatrix2 ((getX canon2),(getY canon2))
    let newMatrix3 = actualizaMatriz newMatrix2 smallMatrix3 (200,20)
    let newMatrix4 = actualizaMatriz newMatrix3 smallMatrix4 (200,30)
    mapM_ putStrLn newMatrix4 --imprime la matriz
    putStrLn (show $ getFuel canon1)
    putStrLn (show $ getFuel canon2)
    if (getFuel canon1 <= 8) 
        then loop (resetFuel canon1) canon2 matrix gen 1
        else do 
            c <- getChar
            case c of
                'a' -> do
                    let newCanon = if (getX canon1) > 12 then  canon1 >>= (moveX (-1)) else canon1 -- Mover barco izquierdo hacia la izquierda
                    loop newCanon canon2 matrix gen 0
                'd' -> do
                    let newCanon = if (getX canon1) < 59 then  canon1 >>= (moveX 1) else canon1 -- Mover barco izquierdo hacia la derecha
                    loop newCanon canon2 matrix gen 0
                'w' -> do
                    let newCanon = if (getAngle canon1) < 3 then  canon1 >>= (moveAngle 1) else canon1 -- Mover ángulo de cañón derecho hacia arriba
                    loop  newCanon canon2 matrix gen 0
                's' -> do
                    let newCanon = if (getAngle canon1) > 0 then  canon1 >>= (moveAngle (-1)) else canon1 -- Mover ángulo de cañón derecho hacia abajo
                    loop newCanon canon2 matrix gen 0
                'p' -> do
                    if ((getFuel canon1) < 20) 
                        then loop canon1 canon2 matrix gen 0
                    else do
                        newCanon <- dispararProyectil canon1 canon2 'r' newMatrix2 gen -- Barco izquierdo dispara proyectil
                        let nextGen = snd (random gen :: (Int, StdGen))
                        loop (resetFuel canon1) newCanon matrix nextGen 1
                't' -> do
                    loop (resetFuel canon1) canon2 matrix gen 1
                'q' -> return ()                -- Salir del bucle
                _  -> loop canon1 canon2 matrix gen 0-- Ignorar otras teclas y repetir
        
loop canon1 canon2 matrix gen 1 = do
    system "clear" -- Limpia la consola
    let smallMatrix1 = tipoBarco 'r' (getAngle canon1)
    let smallMatrix2 = tipoBarco 'l' (getAngle canon2)
    let smallMatrix3 = mostrarDatos canon1 'r'
    let smallMatrix4 = mostrarDatos canon2 'l'
    let newMatrix1 = actualizaMatriz matrix smallMatrix1 ((getX canon1),(getY canon1))
    let newMatrix2 = actualizaMatriz newMatrix1 smallMatrix2 ((getX canon2),(getY canon2))
    let newMatrix3 = actualizaMatriz newMatrix2 smallMatrix3 (200,20)
    let newMatrix4 = actualizaMatriz newMatrix3 smallMatrix4 (200,30)
    mapM_ putStrLn newMatrix4 --imprime la matriz
    putStrLn (show $ getFuel canon1)
    putStrLn (show $ getFuel canon2)
    if (getFuel canon2 <= 8) 
        then loop canon1 (resetFuel canon2) matrix gen 0
        else do 
            c <- getChar
            case c of
                'j' -> do
                    let newCanon = if (getX canon2) > 116 then  canon2 >>= (moveX (-1)) else canon2 -- Mover barco derecho hacia la izquierda
                    loop canon1 newCanon matrix gen 1
                'l' -> do
                    let newCanon = if (getX canon2) < 156 then  canon2 >>= (moveX 1) else canon2 -- Mover barco derecho hacia la derecha
                    loop canon1 newCanon matrix gen 1
                'i' -> do
                    let newCanon = if (getAngle canon2) < 3 then canon2 >>= (moveAngle 1) else canon2 -- Mover ángulo de cañón izquierdo hacia arriba
                    loop canon1 newCanon matrix gen 1
                'k' -> do
                    let newCanon = if (getAngle canon2) > 0 then canon2 >>= (moveAngle (-1)) else canon2 -- Mover ángulo de cañón izquierdo hacia abajo
                    loop canon1 newCanon matrix gen 1
                'o' -> do
                    if (getFuel canon2 < 20) 
                        then loop canon1 canon2 matrix gen 1
                    else do
                        newCanon <- dispararProyectil canon2 canon1 'l' newMatrix2 gen -- Barco derecho dispara proyectil
                        let nextGen = snd (random gen :: (Int, StdGen))
                        loop newCanon (resetFuel canon2) matrix nextGen 0
                't' -> do
                    loop canon1 (resetFuel canon2) matrix gen 0
                'q' -> return ()                -- Salir del bucle
                _  -> loop canon1 canon2 matrix gen 1-- Ignorar otras teclas y repetir
---------------------------------------------------------------------------
main :: IO ()
main = do
    hSetBuffering stdin NoBuffering -- Evitar el buffering en la entrada
    hSetEcho stdin False            -- Desactivar la impresión de teclas
    putStrLn "Presiona teclas (q para salir):"
    gen <- getStdGen
    let canon1 = Canon (30, 100, 20, 43, 0)
    let canon2 = Canon (30, 100, 150, 43, 0)
    let matrix = initialMatrix -- Se invoca la matriz inicial (Graphics.hs)
    let prob = fst (randomR (0,1) gen :: (Int, StdGen))
    loop canon1 canon2 matrix gen prob