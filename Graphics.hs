{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
module Graphics where
import Tools
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
        ["B A R C O  D E R E C H O" 
        , "Vida:        " ++ mostrarVida 30 (getLife canon)
        , "Combustible: " ++ mostrarCombustible 100 (getFuel canon)
        , "Posición: (" ++ show (getX canon) ++ ", " ++ show (getY canon) ++ ")"
        , "Ángulo: " ++ show (getAngle canon)
        ]
    'l' -> 
        ["B A R C O  I Z Q U I E R D O"
        , "Vida:        " ++ mostrarVida 30 (getLife canon)
        , "Combustible: " ++ mostrarCombustible 100 (getFuel canon)
        , "Posición: (" ++ show (getX canon) ++ ", " ++ show (getY canon) ++ ")"
        , "Ángulo: " ++ show (getAngle canon)
        ]

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
