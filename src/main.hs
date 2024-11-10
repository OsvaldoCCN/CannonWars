import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode(NoBuffering))
import Text.Printf (printf)  -- Para formatear salida
import System.Process (system)  -- Para limpiar la consola
import Graphics
import Proyectil
import Tools
import System.Random
import Data.IORef

--------------------------------------------------------------------------------------------------
-- Fucnión que obtiene el input en la pantalla final
getInputFin :: IO()
getInputFin = do
    c <- getChar
    case c of
        'r' -> do
            gen <- getStdGen
            let canon1 = Canon (30, 100, 20, 43, 0)
            let canon2 = Canon (30, 100, 150, 43, 0)
            let matrix = initialMatrix    -- Se invoca la matriz inicial (Graphics.hs)
            let prob = fst (randomR (0,1) gen :: (Int, StdGen))
            loop canon1 canon2 matrix gen prob
        'q' -> return ()                -- Salir del bucle
        _  -> getInputFin

-- Fucnión que obtiene el input en la pantalla principal
getInputTurno :: (Canon (Int, Int, Int, Int, Int)) -> (Canon (Int, Int, Int, Int, Int)) -> [[Char]] -> [[Char]] -> StdGen -> Int -> IO ()
getInputTurno canon1 canon2 matrix newMatrix5 gen 0 = do
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
        'e' -> do
            if ((getFuel canon1) < 20) 
                then loop canon1 canon2 matrix gen 0
            else do
                newCanon <- dispararProyectil canon1 canon2 'r' newMatrix5 gen -- Barco izquierdo dispara proyectil
                let nextGen = snd (random gen :: (Int, StdGen))
                loop (resetFuel canon1) newCanon matrix nextGen 1
        't' -> do
            loop (resetFuel canon1) canon2 matrix gen 1
        'q' -> return ()                -- Salir del bucle
        _ -> getInputTurno canon1 canon2 matrix newMatrix5 gen 0

getInputTurno canon1 canon2 matrix newMatrix5 gen 1 = do
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
                newCanon <- dispararProyectil canon2 canon1 'l' newMatrix5 gen -- Barco derecho dispara proyectil
                let nextGen = snd (random gen :: (Int, StdGen))
                loop newCanon (resetFuel canon2) matrix nextGen 0
        't' -> do
            loop canon1 (resetFuel canon2) matrix gen 0
        'q' -> return ()                -- Salir del bucle
        _  -> getInputTurno canon1 canon2 matrix newMatrix5 gen 1-- Ignorar otras teclas y repetir

-- Función principal que printa la matriz con la información correspondiente 
loop :: (Canon (Int, Int, Int, Int, Int)) -> (Canon (Int, Int, Int, Int, Int)) -> [[Char]] -> StdGen -> Int -> IO ()
-- Barco de la derecha muerto
loop _ Dead _ _ _ = do
    system "clear"
    pantallaFin1
    getInputFin

-- Barco de la izquierda muerto
loop Dead _ _ _ _ = do
    system "clear"
    pantallaFin2
    getInputFin

-- Se ejecuta el turno del barco de la izquierda
loop canon1 canon2 matrix gen 0 = do
    system "clear" -- Limpia la consola
    let barcoIzq = tipoBarco 'r' (getAngle canon1)
    let barcoDer = tipoBarco 'l' (getAngle canon2)
    let datosIzq = mostrarDatos canon1 'r'
    let datosDer = mostrarDatos canon2 'l'
    let turnoMatrix = turno 0
    let newMatrix1 = actualizaMatriz matrix barcoIzq ((getX canon1),(getY canon1))
    let newMatrix2 = actualizaMatriz newMatrix1 barcoDer ((getX canon2),(getY canon2))
    let newMatrix3 = actualizaMatriz newMatrix2 datosIzq (190,20)
    let newMatrix4 = actualizaMatriz newMatrix3 datosDer (188,30)
    let newMatrix5 = actualizaMatriz newMatrix4 turnoMatrix (197,35)
    mapM_ putStrLn newMatrix5 --imprime la matriz
    if (getFuel canon1 <= 8) 
        then loop (resetFuel canon1) canon2 matrix gen 1
        else do 
            getInputTurno canon1 canon2 matrix newMatrix5 gen 0

-- Se ejecuta el turno del barco de la derecha
loop canon1 canon2 matrix gen 1 = do
    system "clear" -- Limpia la consola
    let barcoIzq = tipoBarco 'r' (getAngle canon1)
    let barcoDer = tipoBarco 'l' (getAngle canon2)
    let datosIzq = mostrarDatos canon1 'r'
    let datosDer = mostrarDatos canon2 'l'
    let turnoMatrix = turno 1
    let newMatrix1 = actualizaMatriz matrix barcoIzq ((getX canon1),(getY canon1)) --inserta barco izquierdo
    let newMatrix2 = actualizaMatriz newMatrix1 barcoDer ((getX canon2),(getY canon2)) -- inserta barco derecho
    let newMatrix3 = actualizaMatriz newMatrix2 datosIzq (190,20) -- inserta datos del barco izq
    let newMatrix4 = actualizaMatriz newMatrix3 datosDer (188,30) -- inserta datos del barco izq
    let newMatrix5 = actualizaMatriz newMatrix4 turnoMatrix (196,35) -- inserta info. del turno
    mapM_ putStrLn newMatrix5 --imprime la matriz
    if (getFuel canon2 <= 8) 
        then loop canon1 (resetFuel canon2) matrix gen 0
        else do 
            getInputTurno canon1 canon2 matrix newMatrix5 gen 1
---------------------------------------------------------------------------
main :: IO ()
main = do
    system "clear"
    pantallaInicio
    hSetBuffering stdin NoBuffering -- Evitar el buffering en la entrada
    hSetEcho stdin False            -- Desactivar la impresión de teclas
    gen <- getStdGen
    let canon1 = Canon (30, 100, 20, 43, 0)
    let canon2 = Canon (30, 100, 150, 43, 0)
    let matrix = initialMatrix -- Se invoca la matriz inicial (Graphics.hs)
    let prob = fst (randomR (0,1) gen :: (Int, StdGen)) -- Se genera el primer turno aleatoriamente
    loop canon1 canon2 matrix gen prob -- Se genera el loop inicial