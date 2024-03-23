import Data.Char (isDigit)

-- Verifica se um caractere é um dígito
isNumero :: Char -> Bool
isNumero = isDigit

toInt :: Char -> Int
toInt s = read [s]

-- Preenche a linha com espaços em branco
fillWithSpaces :: Int -> String -> String
fillWithSpaces n str = str ++ replicate (n - length str) ' '

-- Converte uma linha da lista para a notação Forsyth-Edwards
lineToForsyth :: String -> String
lineToForsyth [] = []
lineToForsyth (a:x)
    | isNumero a = replicate (toInt a) ' ' ++ lineToForsyth x
    | otherwise = a : lineToForsyth x

-- Converte a lista para a matriz na notação Forsyth-Edwards
listToMatrixForsyth :: [String] -> [String]
listToMatrixForsyth [] = []
listToMatrixForsyth (x:xs) = lineToForsyth x : listToMatrixForsyth xs

-- Imprime a matriz na notação Forsyth-Edwards
printMatrixForsyth :: [String] -> IO ()
printMatrixForsyth = mapM_ putStrLn

findKinginLine :: Int -> Int -> [Char] -> [Int]
findKinginLine _ _ [] = [-1, -1]
findKinginLine i j (a:x)
  | a == 'r' = [i, j]
  | otherwise = findKinginLine i (j + 1) x

----ENCONTRAR O REI BRANCO 'r'--------
findWhiteKing :: Int -> [String] -> [Int]
findWhiteKing _ [] = [-1, -1] -- Retorna [-1, -1] se o rei não for encontrado na matriz
findWhiteKing i (l1:x)
    | kingPos /= [-1, -1] = kingPos -- Retorna a posição do rei se encontrado em uma linha
    | otherwise = findWhiteKing (i + 1) x -- Continua procurando na próxima linha
    where
      kingPos = findKinginLine (i + 1) 0 l1

main :: IO ()
main = do
    let lista = ["tcbdrbct","pppppppp","8","8","8","8","PPPPPPPP","TCBDRBCT"]
        matriz = listToMatrixForsyth lista
        whiteKingPos = findWhiteKing (-1) matriz
    printMatrixForsyth matriz
    
    putStrLn $ "\nPosição do rei branco: " ++ show (whiteKingPos)
    
