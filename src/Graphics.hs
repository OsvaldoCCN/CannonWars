{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
module Graphics where
import Tools
import System.IO (hFlush, stdout)
import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode(NoBuffering))
--CONSTANTES

-- Función que actualiza la matriz con los barcos
actualizaMatriz :: [[Char]] -> [[Char]] -> (Int,Int)-> [[Char]]
actualizaMatriz bigMatrix smallMatrix (x,y) =
    zipWith replaceRow [0..] bigMatrix
  where
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

-- Función auxiliar para crear una barra de vida visual con colores.
mostrarVida :: Int -> Int -> String
mostrarVida maxVida vida = 
  let vidaPorcentaje = (vida * 100) `div` maxVida
      vidaBarra = vidaPorcentaje `div` 5  -- Cambiado para tener más barras
      vidaRestante = 20 - vidaBarra
      color = if vidaPorcentaje <= 20 then "\x1b[31m"  -- Rojo para poca vida
              else if vidaPorcentaje <= 50 then "\x1b[33m"  -- Amarillo para vida media
              else "\x1b[32m"  -- Verde para vida alta
  in color ++ replicate vidaBarra '█' ++ "\x1b[0m" ++ replicate vidaRestante '░' ++ " " ++ show vidaPorcentaje ++ "%" ++ " (" ++ show vida ++ ")"

-- Función auxiliar para crear una barra de combustible visual con colores.
mostrarCombustible :: Int -> Int -> String
mostrarCombustible maxFuel fuel = 
  let fuelPorcentaje = (fuel * 100) `div` maxFuel
      fuelBarra = fuelPorcentaje `div` 5  -- Cambiado para tener más barras
      fuelRestante = 20 - fuelBarra
  in "\x1b[33m" ++ replicate fuelBarra '█' ++ "\x1b[0m" ++ replicate fuelRestante '░' ++ " " ++ show fuelPorcentaje ++ "%" ++ " (" ++ show fuel ++ ")"


-- Función mostrarDatos actualizada para usar las barras con colores.
mostrarDatos :: Canon (Int, Int, Int, Int, Int) -> Char -> [[Char]]
mostrarDatos canon c = case c of
    'r' ->
        ["B A R C O  I Z Q U I E R D O" 
        , "Vida:        " ++ mostrarVida 30 (getLife canon)
        , "Combustible: " ++ mostrarCombustible 100 (getFuel canon)
        ]
    'l' -> 
        ["B A R C O   D E R E C H O"
        , "Vida:        " ++ mostrarVida 30 (getLife canon)
        , "Combustible: " ++ mostrarCombustible 100 (getFuel canon)
        ]

turno :: Int  -> [[Char]]
turno 1 = ["Turno Actual : B A R C O   D E R E C H O"]
turno 0 = ["Turno Actual : B A R C O  I Z Q U I E R D O"]

-- Formatos barcos
tipoBarco :: Char -> Int -> [[Char]]
tipoBarco h limitedA = case (h, limitedA) of
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



--Matriz inicial
initialMatrix :: [[Char]]
initialMatrix =  
          [ "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |       Costos de Combustible:               "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |         Mover Barco    = 8u               "
          , "|                                                                                                                                                                       |         Mover Cañon    = 8u               "
          , "|                                                                                                                                                                       |         Disparar Cañon = 20u              "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                                                                                                       |                                            "
          , "|                                                                                     ▒▒▒▒▒                                                                             |                                            "
          , "|                                                                                   ▒▒▒▒▒▒▒▒▒▒▒                                                                         |                                            "
          , "|                                                                                 ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                       |                                            "
          , "|                                                                               ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                      |                                            "
          , "|                                                                               ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                    |                                            "
          , "|                                                                              ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                   |                                            "
          , "|                                                                              ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                  |                                            "
          , "|                                                                           ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                 |                                            "
          , "|                                                                           ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                 |                                            "
          , "|                                                                          ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                 |                                            "
          , "|                                                                          ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                                 |                                            "
          , "|                                                                        ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                               |                                            "
          , "|                                                                      ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                                                               |                                            "
          , "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░                                            "
          , "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░                                            "
          , "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░                                            "
          ]


pantallaFin1 :: IO()
pantallaFin1 = do
    putStrLn "|                                                                                                                                                                       |===================================="
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                  ██████    ████    ██      ██  ████████                                                                                               |"
    putStrLn "|                                ██        ██    ██  ████  ████  ██                                                                                                     |"
    putStrLn "|                                ██  ████  ████████  ██  ██  ██  ██████                                                                                                 |"
    putStrLn "|                                ██    ██  ██    ██  ██      ██  ██                                                                                                     | Instrucciones:                     "
    putStrLn "|                                  ██████  ██    ██  ██      ██  ████████                                                                                               | - Teclas barco izquierdo:          "
    putStrLn "|                                                                                                                                                                       |   - 'a' Mover a la izquierda       "
    putStrLn "|                                                     ████    ██      ██  ████████  ██████                                                                              |   - 'd' Mover a la derecha         "
    putStrLn "|                                                   ██    ██  ██      ██  ██        ██    ██                                                                            |   - 'w' Subir cañón                "
    putStrLn "|                                                   ██    ██  ██      ██  ██████    ██████                                                                              |   - 's' Bajar cañón                "
    putStrLn "|                                                   ██    ██    ██  ██    ██        ██    ██                                                                            |   - 'e' Disparar proyectil         "
    putStrLn "|                                                     ████        ██      ████████  ██    ██                                                                            |"
    putStrLn "|                                                                                                                                                                       | - Teclas barco derecho:            "
    putStrLn "|                                                                                                                                                                       |   - 'j' Mover a la izquierda       "
    putStrLn "|                                                                                                                                                                       |   - 'l' Mover a la derecha       "
    putStrLn "|                                                                                                                                                                       |   - 'i' Subir cañon       "
    putStrLn "|                                                                                                                                                                       |   - 'k' bajar cañon      "
    putStrLn "|                                                                                                                                                                       |   - 'o' Disparar Cañon       "
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |  - 'q' Para salir"
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |  - 't' Para cambiar de turno" 
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|           \ESC[38;5;240m██████████████\ESC[0m                                                                                                                                              |"
    putStrLn "|     \ESC[38;5;240m████████\ESC[38;5;226m██████████\ESC[38;5;240m████████\ESC[0m                                                                                                                                        |"
    putStrLn "|     \ESC[38;5;240m██\ESC[0m    \ESC[38;5;240m██\ESC[38;5;226m████\ESC[38;5;231m████\ESC[38;5;226m██\ESC[38;5;240m██\ESC[0m    \ESC[38;5;240m██\ESC[0m                                                                                                   \ESC[38;5;244m██\ESC[38;5;240m██\ESC[38;5;244m██\ESC[38;5;240m██████\ESC[38;5;244m████\ESC[0m                     |"
    putStrLn "|     \ESC[38;5;240m██\ESC[0m    \ESC[38;5;240m██\ESC[38;5;226m██████\ESC[38;5;231m██\ESC[38;5;226m██\ESC[38;5;240m██\ESC[0m    \ESC[38;5;240m██\ESC[0m                                                                                                   \ESC[38;5;240m██████\ESC[38;5;244m████████\ESC[38;5;240m██\ESC[0m                     |"
    putStrLn "|       \ESC[38;5;240m██\ESC[0m  \ESC[38;5;240m██\ESC[38;5;226m██████████\ESC[38;5;240m██\ESC[0m  \ESC[38;5;240m██\ESC[0m                                                                                                     \ESC[38;5;240m██\ESC[38;5;251m████████████\ESC[38;5;244m██\ESC[0m                     |"
    putStrLn "|         \ESC[38;5;240m████\ESC[38;5;226m██████████\ESC[38;5;240m████\ESC[0m                                                                                                       \ESC[38;5;251m████████\ESC[38;5;244m██\ESC[38;5;251m██\ESC[38;5;244m████\ESC[0m                     |"
    putStrLn "|             \ESC[38;5;240m██\ESC[38;5;226m██████\ESC[38;5;240m██\ESC[0m                                                                                                           \ESC[38;5;251m██\ESC[38;5;234m████\ESC[38;5;251m████\ESC[38;5;234m████\ESC[38;5;251m██\ESC[0m                     |"
    putStrLn "|               \ESC[38;5;240m██\ESC[38;5;226m██\ESC[38;5;240m██\ESC[0m                                                                                                             \ESC[38;5;244m██\ESC[38;5;251m████\ESC[38;5;240m████\ESC[38;5;251m██\ESC[38;5;244m████\ESC[0m                     |"
    putStrLn "|               \ESC[38;5;240m██\ESC[38;5;226m██\ESC[38;5;240m██\ESC[0m                                                                                                             \ESC[38;5;244m██\ESC[38;5;234m████████████\ESC[38;5;244m██\ESC[0m                     |"
    putStrLn "|               \ESC[38;5;240m██\ESC[38;5;226m██\ESC[38;5;240m██\ESC[0m                                                                                                             \ESC[38;5;244m████████\ESC[38;5;240m██\ESC[38;5;244m██\ESC[38;5;240m██\ESC[38;5;244m██\ESC[0m                     |"
    putStrLn "|             \ESC[38;5;240m██\ESC[38;5;226m██████\ESC[38;5;240m██\ESC[0m                                                                                                                                                |"
    putStrLn "|           \ESC[38;5;240m██\ESC[38;5;226m██████████\ESC[38;5;240m██\ESC[0m                                                                                                                                              |"
    putStrLn "|           \ESC[38;5;240m██████████████\ESC[0m                                                                                                                                              |"
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |"

pantallaFin2 :: IO()
pantallaFin2 = do
    putStrLn "|                                                                                                                                                                       |===================================="
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                  ██████    ████    ██      ██  ████████                                                                                               |"
    putStrLn "|                                ██        ██    ██  ████  ████  ██                                                                                                     |"
    putStrLn "|                                ██  ████  ████████  ██  ██  ██  ██████                                                                                                 |"
    putStrLn "|                                ██    ██  ██    ██  ██      ██  ██                                                                                                     | Instrucciones:                     "
    putStrLn "|                                  ██████  ██    ██  ██      ██  ████████                                                                                               | - Teclas barco izquierdo:          "
    putStrLn "|                                                     ████    ██      ██  ████████  ██████                                                                              |   - 'a' Mover a la izquierda       "
    putStrLn "|                                                                                                                                                                       |   - 'd' Mover a la derecha         "
    putStrLn "|                                                     ████    ██      ██  ████████  ██████                                                                              |   - 'w' Subir cañón                "
    putStrLn "|                                                   ██    ██  ██      ██  ██        ██    ██                                                                            |   - 's' Bajar cañón                "
    putStrLn "|                                                   ██    ██  ██      ██  ██████    ██████                                                                              |   - 'e' Disparar proyectil         "
    putStrLn "|                                                   ██    ██    ██  ██    ██        ██    ██                                                                            |"
    putStrLn "|                                                     ████        ██      ████████  ██    ██                                                                            | - Teclas barco derecho:            "
    putStrLn "|                                                                                                                                                                       |  - 'j' Mover a la izquierda        "
    putStrLn "|                                                                                                                                                                       |  - 'l' Mover a la derecha          "
    putStrLn "|                                                                                                                                                                       |  - 'i' Subir cañon                 "
    putStrLn "|                                                                                                                                                                       |  - 'k' bajar cañon                 "
    putStrLn "|                                                                                                                                                                       |  - 'o' Disparar Cañon              "
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |  - 'q' Para salir"
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |  - 'r' Para volver a jugar"
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |" 
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                       \ESC[38;5;240m██████████████\ESC[0m                  |"
    putStrLn "|                                                                                                                                 \ESC[38;5;240m████████\ESC[38;5;226m██████████\ESC[38;5;240m████████\ESC[0m            |"
    putStrLn "|           \ESC[38;5;244m██\ESC[38;5;240m██\ESC[38;5;244m██\ESC[38;5;240m██████\ESC[38;5;244m████\ESC[0m                                                                                                      \ESC[38;5;240m██\ESC[0m    \ESC[38;5;240m██\ESC[38;5;226m████\ESC[38;5;231m████\ESC[38;5;226m██\ESC[38;5;240m██\ESC[0m    \ESC[38;5;240m██\ESC[0m            |"
    putStrLn "|           \ESC[38;5;240m██████\ESC[38;5;244m████████\ESC[38;5;240m██\ESC[0m                                                                                                      \ESC[38;5;240m██\ESC[0m    \ESC[38;5;240m██\ESC[38;5;226m██████\ESC[38;5;231m██\ESC[38;5;226m██\ESC[38;5;240m██\ESC[0m    \ESC[38;5;240m██\ESC[0m            |"
    putStrLn "|           \ESC[38;5;240m██\ESC[38;5;251m████████████\ESC[38;5;244m██\ESC[0m                                                                                                        \ESC[38;5;240m██\ESC[0m  \ESC[38;5;240m██\ESC[38;5;226m██████████\ESC[38;5;240m██\ESC[0m  \ESC[38;5;240m██\ESC[0m              |"
    putStrLn "|           \ESC[38;5;251m████████\ESC[38;5;244m██\ESC[38;5;251m██\ESC[38;5;244m████\ESC[0m                                                                                                          \ESC[38;5;240m████\ESC[38;5;226m██████████\ESC[38;5;240m████\ESC[0m                |"
    putStrLn "|           \ESC[38;5;251m██\ESC[38;5;234m████\ESC[38;5;251m████\ESC[38;5;234m████\ESC[38;5;251m██\ESC[0m                                                                                                              \ESC[38;5;240m██\ESC[38;5;226m██████\ESC[38;5;240m██\ESC[0m                    |"
    putStrLn "|           \ESC[38;5;244m██\ESC[38;5;251m████\ESC[38;5;240m████\ESC[38;5;251m██\ESC[38;5;244m████\ESC[0m                                                                                                                \ESC[38;5;240m██\ESC[38;5;226m██\ESC[38;5;240m██\ESC[0m                      |"
    putStrLn "|           \ESC[38;5;244m██\ESC[38;5;234m████████████\ESC[38;5;244m██\ESC[0m                                                                                                                \ESC[38;5;240m██\ESC[38;5;226m██\ESC[38;5;240m██\ESC[0m                      |"
    putStrLn "|           \ESC[38;5;244m████████\ESC[38;5;240m██\ESC[38;5;244m██\ESC[38;5;240m██\ESC[38;5;244m██\ESC[0m                                                                                                                \ESC[38;5;240m██\ESC[38;5;226m██\ESC[38;5;240m██\ESC[0m                      |"
    putStrLn "|                                                                                                                                         \ESC[38;5;240m██\ESC[38;5;226m██████\ESC[38;5;240m██\ESC[0m                    |"
    putStrLn "|                                                                                                                                       \ESC[38;5;240m██\ESC[38;5;226m██████████\ESC[38;5;240m██\ESC[0m                  |"
    putStrLn "|                                                                                                                                       \ESC[38;5;240m██████████████\ESC[0m                  |"
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |"
    putStrLn "|                                                                                                                                                                       |"


pantallaInicio :: IO()
pantallaInicio = do
  putStrLn "|                                                                                                                                                                       |===================================="
  putStrLn "|                                                                  ████    ██  ███████  ██     ██  ███████  ██                                                          |     \ESC[32m¡Bienvenido a CannonWars!\ESC[0m      "
  putStrLn "|                                                                  ██ ██   ██  ██   ██   ██   ██   ██   ██  ██                                                          |"
  putStrLn "|                                                                  ██  ██  ██  ███████   ██   ██   ███████  ██                                                          |   El objetivo del juego es destruir"
  putStrLn "|                                                                  ██   ██ ██  ██   ██    ██ ██    ██   ██  ██                                                          |   el barco enemigo antes de que él "
  putStrLn "|                                                                  ██    ████  ██   ██     ███     ██   ██  █████                                                       |   te destruya a ti.                "
  putStrLn "|                                                                                                                                                                       |"
  putStrLn "|                                                                 ███████  ███████   ██████    ██  ██    ██  █████                                                      |"
  putStrLn "|                                                                 ██          ██     ██   ██   ██  ██  ██    ██                                                         | Instrucciones:                     "
  putStrLn "|                                                                 ███████     ██     ██████    ██  ████      ████                                                       | - Teclas barco izquierdo:          "
  putStrLn "|                                                                      ██     ██     ██   ██   ██  ██  ██    ██                                                         |   - 'a' Mover a la izquierda       "  
  putStrLn "|                                                                 ███████     ██     ██    ██  ██  ██    ██  █████                                                      |   - 'd' Mover a la derecha         " 
  putStrLn "|                                                                                                                                                                       |   - 'w' Subir cañón                "
  putStrLn "|                                                                                                                                                                       |   - 's' Bajar cañón                "
  putStrLn "|                                                                                                                                                                       |   - 'e' Disparar proyectil         "
  putStrLn "|                                                                                                                                                                       |"
  putStrLn "|                                                                                                                                                                       | - Teclas barco derecho:            "
  putStrLn "|                                                            \ESC[38;5;240m██\ESC[0m                                                                                                         |   - 'j' Mover a la izquierda       "
  putStrLn "|                                                          \ESC[38;5;240m██\ESC[38;5;250m██\ESC[38;5;240m██\ESC[0m                                                                                                       |   - 'l' Mover a la derecha         "
  putStrLn "|                                                        \ESC[38;5;240m██\ESC[38;5;250m██████\ESC[38;5;240m██\ESC[0m                                                                                                     |   - 'i' Subir cañón                "
  putStrLn "|                                                        \ESC[38;5;240m██\ESC[38;5;26m██\ESC[38;5;250m██████\ESC[38;5;240m██\ESC[0m                                                                                                   |   - 'k' Bajar cañón                "
  putStrLn "|                                                      \ESC[38;5;240m██\ESC[38;5;26m██████\ESC[38;5;250m██\ESC[38;5;252m████\ESC[38;5;240m██\ESC[0m                                                                                                 |   - 'o' Disparar proyectil         "
  putStrLn "|                                                      \ESC[38;5;240m██\ESC[38;5;26m████\ESC[38;5;17m████\ESC[38;5;252m██████\ESC[38;5;240m██\ESC[0m                                                                                               |"
  putStrLn "|                                                      \ESC[38;5;240m██\ESC[38;5;26m████\ESC[38;5;17m██████\ESC[38;5;252m██████\ESC[38;5;240m██\ESC[0m                          \ESC[38;5;240m██\ESC[0m                                                                 |"
  putStrLn "|                                                      \ESC[38;5;240m██\ESC[38;5;26m██\ESC[38;5;17m██████████\ESC[38;5;252m██████\ESC[38;5;240m██\ESC[0m                      \ESC[38;5;240m██\ESC[38;5;17m██\ESC[38;5;240m██\ESC[0m                                                               |  - 'q' Salir del juego            "
  putStrLn "|                                                      \ESC[38;5;240m██\ESC[38;5;26m██\ESC[38;5;17m████████████\ESC[38;5;252m██████\ESC[38;5;240m██\ESC[0m        \ESC[38;5;240m██\ESC[0m        \ESC[38;5;240m██\ESC[38;5;250m██\ESC[38;5;17m████\ESC[38;5;240m██\ESC[0m                                                             |  - 't' Finalizar turno"
  putStrLn "|                                              \ESC[38;5;240m██████████\ESC[38;5;26m██\ESC[38;5;17m██████████████\ESC[38;5;252m██████\ESC[38;5;240m██\ESC[0m    \ESC[38;5;240m██\ESC[38;5;231m██\ESC[38;5;240m██\ESC[0m    \ESC[38;5;240m██\ESC[38;5;250m██████\ESC[38;5;26m██\ESC[38;5;240m██\ESC[0m                                                             |  Presiona ENTER para comenzar...  "
  putStrLn "|                                            \ESC[38;5;240m██\ESC[38;5;88m██████\ESC[38;5;240m████\ESC[38;5;26m██\ESC[38;5;17m████████████████\ESC[38;5;252m██████\ESC[38;5;240m████\ESC[38;5;231m██\ESC[38;5;250m██\ESC[38;5;231m██\ESC[38;5;240m████\ESC[38;5;252m██\ESC[38;5;250m████\ESC[38;5;231m██\ESC[38;5;240m██\ESC[0m                                                               |"
  putStrLn "|                                          \ESC[38;5;240m██\ESC[38;5;124m██\ESC[38;5;88m████\ESC[38;5;240m██\ESC[0m  \ESC[38;5;240m██\ESC[38;5;17m████████████████████\ESC[38;5;252m████████████\ESC[38;5;250m██\ESC[38;5;231m██\ESC[38;5;252m████\ESC[38;5;250m████\ESC[38;5;231m██\ESC[38;5;240m██\ESC[0m                                                               |"
  putStrLn "|                                          \ESC[38;5;240m████\ESC[38;5;88m██\ESC[38;5;240m████\ESC[0m  \ESC[38;5;240m██\ESC[38;5;88m██\ESC[38;5;17m████████████████████\ESC[38;5;252m██████\ESC[38;5;17m██\ESC[38;5;252m████\ESC[38;5;250m██\ESC[38;5;231m██\ESC[38;5;252m██\ESC[38;5;250m██\ESC[38;5;231m██\ESC[38;5;240m██\ESC[0m          \ESC[38;5;240m██\ESC[0m                                                     |"
  putStrLn "|                                            \ESC[38;5;240m██████\ESC[0m  \ESC[38;5;240m████\ESC[38;5;88m████\ESC[38;5;17m████████████████████\ESC[38;5;252m██████\ESC[38;5;17m██\ESC[38;5;252m████\ESC[38;5;250m██\ESC[38;5;231m██\ESC[38;5;250m██\ESC[38;5;231m██\ESC[38;5;240m██\ESC[0m        \ESC[38;5;240m██\ESC[38;5;17m██\ESC[38;5;240m██\ESC[0m                                                   |"
  putStrLn "|                                              \ESC[38;5;240m██\ESC[38;5;124m██\ESC[38;5;240m████\ESC[38;5;88m████████\ESC[38;5;17m████████████████████\ESC[38;5;252m████████████\ESC[38;5;250m██\ESC[38;5;231m██\ESC[38;5;240m██\ESC[0m        \ESC[38;5;240m██\ESC[38;5;250m██\ESC[38;5;17m████\ESC[38;5;240m██\ESC[0m                                                 |"
  putStrLn "|                                                \ESC[38;5;240m██\ESC[38;5;124m████\ESC[38;5;88m██████████\ESC[38;5;17m████████████████████\ESC[38;5;252m██████\ESC[38;5;17m██\ESC[38;5;252m████\ESC[38;5;250m██\ESC[38;5;231m██\ESC[38;5;240m██\ESC[0m    \ESC[38;5;240m██\ESC[38;5;250m██████\ESC[38;5;26m██\ESC[38;5;240m██\ESC[0m                                                 |"
  putStrLn "|                                                  \ESC[38;5;240m██\ESC[38;5;124m██\ESC[38;5;88m████████████\ESC[38;5;17m████████████████████\ESC[38;5;252m██████\ESC[38;5;17m██\ESC[38;5;252m████\ESC[38;5;250m██\ESC[38;5;231m██\ESC[38;5;240m████\ESC[38;5;252m██\ESC[38;5;250m████\ESC[38;5;231m██\ESC[38;5;240m██\ESC[0m                                                   |"
  putStrLn "|                                                    \ESC[38;5;240m██\ESC[38;5;124m██\ESC[38;5;88m████████████\ESC[38;5;17m████████████████████\ESC[38;5;252m██████\ESC[38;5;17m██\ESC[38;5;252m████\ESC[38;5;250m██\ESC[38;5;231m██\ESC[38;5;252m████\ESC[38;5;250m████\ESC[38;5;231m██\ESC[38;5;240m██\ESC[0m                                                   |"
  putStrLn "|                                                      \ESC[38;5;240m██\ESC[38;5;124m██\ESC[38;5;88m████████████\ESC[38;5;17m████████████████████\ESC[38;5;252m████████████\ESC[38;5;250m██\ESC[38;5;231m██\ESC[38;5;252m██\ESC[38;5;250m██\ESC[38;5;231m██\ESC[38;5;240m██\ESC[0m                                                     |"
  putStrLn "|                                                        \ESC[38;5;240m██\ESC[38;5;124m██\ESC[38;5;88m████████████\ESC[38;5;17m████████████████████\ESC[38;5;252m██████\ESC[38;5;17m██\ESC[38;5;252m████\ESC[38;5;250m██\ESC[38;5;231m██\ESC[38;5;250m██\ESC[38;5;231m██\ESC[38;5;240m██\ESC[0m                                                     |"
  putStrLn "|                              \ESC[38;5;38m██████\ESC[38;5;31m████\ESC[38;5;38m██\ESC[38;5;31m██████████████████████████████████████████████████████████████████████████████\ESC[38;5;38m██████████\ESC[0m                                     |"
  putStrLn "|                                  \ESC[38;5;31m████\ESC[38;5;38m████████\ESC[38;5;31m██████\ESC[38;5;38m████\ESC[38;5;31m██████████████\ESC[38;5;38m██████████████████\ESC[38;5;31m████\ESC[38;5;38m██████\ESC[38;5;31m██\ESC[38;5;38m████\ESC[38;5;31m████████████████████████\ESC[0m                                       |"
  putStrLn "|                                    \ESC[38;5;38m████████████████████████████████████████████████████████████████████████████████████████\ESC[0m                                           |"

  hSetEcho stdin False            -- Desactivar la impresión de teclas
  hFlush stdout  -- Asegura que el mensaje se muestre antes de esperar la entrada
  _ <- getLine  -- Espera a que el usuario presione ENTER
  return ()



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
