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

mostrarDatos :: Canon (Int, Int, Int, Int, Int) -> Char -> [[Char]]
mostrarDatos canon c = case c of
    'r' ->
        ["B A R C O  D E R E C H O" 
        ,"Vida: " ++ show (getLife canon)
        , "Combustible: " ++ show (getFuel canon)
        , "Posición: (" ++ show (getX canon) ++ ", " ++ show (getY canon) ++ ")"
        , "Ángulo: " ++ show (getAngle canon)
        ]
    'l' -> 
        ["B A R C O  I Z Q U I E R D O"
        ,"Vida: " ++ show (getLife canon)
        , "Combustible: " ++ show (getFuel canon)
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
