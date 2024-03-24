import Data.Char (isDigit)

-- Verifica se um caractere é um dígito
isNumero :: Char -> Bool
isNumero = isDigit

toInt :: Char -> Int
toInt s = read [s]

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

isPosNull :: [Int] -> Bool
isPosNull num
  | num == [-1, -1] = True
  | otherwise = False

--ENCONTRA TORRE OU RAINHA NA VERTICAL E HORIZONTAL
sobe :: Int -> Int -> [String] -> Bool
sobe x y matriz
  | x >= 8 || x < 0 || y >= 8 || y < 0 = False
  | matriz !! x !! y == 'T' || matriz !! x !! y == 'R' = True
  | matriz !! x !! y /= ' ' && matriz !! x !! y /= ' ' = False
  | otherwise = sobe (x + 1) y matriz

desce :: Int -> Int -> [String] -> Bool
desce x y matriz
  | x >= 8 || x < 0 || y >= 8 || y < 0 = False
  | matriz !! x !! y == 'T' || matriz !! x !! y == 'R' = True
  | matriz !! x !! y /= ' ' && matriz !! x !! y /= ' ' = False
  | otherwise = desce (x - 1) y matriz

direita :: Int -> Int -> [String] -> Bool
direita x y matriz
  | x >= 8 || x < 0 || y >= 8 || y < 0 = False
  | matriz !! x !! y == 'T' || matriz !! x !! y == 'R' = True
  | matriz !! x !! y /= ' ' && matriz !! x !! y /= ' ' = False
  | otherwise = direita x (y + 1) matriz

esquerda :: Int -> Int -> [String] -> Bool
esquerda x y matriz
  | x >= 8 || x < 0 || y >= 8 || y < 0 = False
  | matriz !! x !! y == 'T' || matriz !! x !! y == 'R' = True
  | matriz !! x !! y /= ' ' && matriz !! x !! y /= ' ' = False
  | otherwise = esquerda x (y - 1) matriz

findReto :: [Int] -> [String] -> Bool
findReto (x:y:_) matriz = sobe (x + 1) y matriz || desce (x - 1) y matriz || direita x (y + 1) matriz ||esquerda x (y - 1) matriz

--ENCONTRA BISPOS OU RAINHA NAS DIAGONAIS
--findDiagonal :: [Int] -> [String] -> Bool
--findDiagonal

--ENCONTRA CAVALO
encontraCavalo :: Int -> Int -> [String] -> Bool
encontraCavalo x y matriz
  | x >= 8 || x < 0 || y >= 8 || y < 0 = False
  | matriz !! x !! y == 'C' = True
  | otherwise = False

findHorse :: [Int] -> [String] -> Bool
findHorse (x:y:_) matriz = encontraCavalo (x - 2) (y + 1) matriz || encontraCavalo (x - 1) (y + 2) matriz || encontraCavalo (x + 1) (y + 2) matriz || encontraCavalo (x + 2) (y + 1) matriz || encontraCavalo (x + 2) (y - 1) matriz || encontraCavalo (x + 1) (y - 2) matriz || encontraCavalo (x - 1) (y - 2) matriz || encontraCavalo (x - 2) (y - 1) matriz

--ENCONTRA PEÃO



main :: IO ()
main = do
    let lista = ["tcbdrbct","pppppppp","8","8","8","8","PPPPPPPP","TCBDRBCT"]
        matriz = listToMatrixForsyth lista
        whiteKingPos = findWhiteKing (-1) matriz
    printMatrixForsyth matriz
    
    --Verifica se tem rei no tabuleiro
    if isPosNull whiteKingPos then
      putStrLn $ "False"
      
    --Verifica se há um cavalo que possa dar xeque  
    else if findHorse whiteKingPos matriz then
      putStrLn $ "True"
    
    --Verifica se há uma peça que possa dar xeque na horizontal ou vertical
    else if findReto whiteKingPos matriz then
      putStrLn $ "True"
    
    else
      putStrLn $ "False"
    putStrLn $ "\nPosição do rei branco: " ++ show (whiteKingPos)
    
