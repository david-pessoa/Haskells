import Data.Char (isDigit)

-- Verifica se um caractere é um dígito
isNumero :: Char -> Bool
isNumero = isDigit

-- Preenche a linha com espaços em branco
fillWithSpaces :: Int -> String -> String
fillWithSpaces n str = str ++ replicate (n - length str) ' '

-- Converte uma linha da lista para a notação Forsyth-Edwards
lineToForsyth :: String -> String
lineToForsyth line
    | all isNumero line = replicate (read line) ' '
    | otherwise = line

-- Converte a lista para a matriz na notação Forsyth-Edwards
listToMatrixForsyth :: [String] -> [String]
listToMatrixForsyth [] = []
listToMatrixForsyth (x:xs) = lineToForsyth x : listToMatrixForsyth xs

-- Imprime a matriz na notação Forsyth-Edwards
printMatrixForsyth :: [String] -> IO ()
printMatrixForsyth = mapM_ putStrLn

main :: IO ()
main = do
    let lista = ["tcbdrbct","pppppppp","8","8","8","8","PPPPPPPP","TCBDRBCT"]
        matriz = listToMatrixForsyth lista
    printMatrixForsyth matriz
