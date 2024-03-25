import Data.Char (isDigit)

-- Verifica se um caractere é um dígito
isNumero :: Char -> Bool
isNumero = isDigit

--Converte um caractere para inteiro
toInt :: Char -> Int
toInt s = read [s]

-- Converte cada linha da matriz na notação Forsyth-Edwards para uma linha mais parecida com a de um tabuleiro
lineToForsyth :: String -> String -- na nova matriz, os números são substituídos por espaços vazios
lineToForsyth [] = []
lineToForsyth (a:x)
    | isNumero a = replicate (toInt a) ' ' ++ lineToForsyth x
    | otherwise = a : lineToForsyth x

-- Converte a lista a matriz na notação Forsyth-Edwards para uma matriz que pareça um tabuleiro
listToMatrixForsyth :: [String] -> [String]
listToMatrixForsyth [] = []
listToMatrixForsyth (x:xs) = lineToForsyth x : listToMatrixForsyth xs

-- Imprime a matriz na notação Forsyth-Edwards
printMatrixForsyth :: [String] -> IO ()
printMatrixForsyth = mapM_ putStrLn

----ENCONTRAR O REI BRANCO 'r'--------
--Verifica se o rei está na linha i
findKinginLine :: Int -> Int -> [Char] -> [Int]
findKinginLine _ _ [] = [-1, -1]
findKinginLine i j (a:x)
  | a == 'r' = [i, j]
  | otherwise = findKinginLine i (j + 1) x

--Encontra a posição do rei na matriz
findWhiteKing :: Int -> [String] -> [Int]
findWhiteKing _ [] = [-1, -1] -- Retorna [-1, -1] se o rei não for encontrado na matriz
findWhiteKing i (l1:x)
    | kingPos /= [-1, -1] = kingPos -- Retorna a posição do rei se encontrado em uma linha
    | otherwise = findWhiteKing (i + 1) x -- Continua procurando na próxima linha
    where
      kingPos = findKinginLine (i + 1) 0 l1

isPosNull :: [Int] -> Bool --Verifica se a coordenada fornecida é inválida
isPosNull num
  | num == [-1, -1] = True
  | otherwise = False

--ENCONTRA TORRE OU RAINHA NA VERTICAL E HORIZONTAL
--Verifica se há uma torre ou rainha na parte superior da coluna em que está o rei
sobe :: Int -> Int -> [String] -> Bool
sobe x y matriz
  | x >= 8 || x < 0 || y >= 8 || y < 0 = False
  | matriz !! x !! y == 'T' || matriz !! x !! y == 'R' = True
  | matriz !! x !! y /= ' ' && matriz !! x !! y /= ' ' = False
  | otherwise = sobe (x + 1) y matriz

--Verifica se há uma torre ou rainha na parte inferior da coluna em que está o rei
desce :: Int -> Int -> [String] -> Bool
desce x y matriz
  | x >= 8 || x < 0 || y >= 8 || y < 0 = False
  | matriz !! x !! y == 'T' || matriz !! x !! y == 'R' = True
  | matriz !! x !! y /= ' ' && matriz !! x !! y /= ' ' = False
  | otherwise = desce (x - 1) y matriz

--Verifica se há uma torre ou rainha à direita da linha em que está o rei
direita :: Int -> Int -> [String] -> Bool
direita x y matriz
  | x >= 8 || x < 0 || y >= 8 || y < 0 = False
  | matriz !! x !! y == 'T' || matriz !! x !! y == 'R' = True
  | matriz !! x !! y /= ' ' && matriz !! x !! y /= ' ' = False
  | otherwise = direita x (y + 1) matriz

--Verifica se há uma torre ou rainha à esquerda da linha em que está o rei
esquerda :: Int -> Int -> [String] -> Bool
esquerda x y matriz
  | x >= 8 || x < 0 || y >= 8 || y < 0 = False
  | matriz !! x !! y == 'T' || matriz !! x !! y == 'R' = True
  | matriz !! x !! y /= ' ' && matriz !! x !! y /= ' ' = False
  | otherwise = esquerda x (y - 1) matriz

--Verifica se há alguma torre ou rainha nas diagonais
findReto :: [Int] -> [String] -> Bool
findReto (x:y:_) matriz = sobe (x + 1) y matriz || desce (x - 1) y matriz || direita x (y + 1) matriz ||esquerda x (y - 1) matriz

--ENCONTRA BISPOS OU RAINHA NAS DIAGONAIS
--Verifica se há um bispo ou rainha na diagonal superior direita
nordeste :: Int -> Int -> [String] -> Bool
nordeste x y matriz
  | x >= 8 || x < 0 || y >= 8 || y < 0 = False
  | matriz !! x !! y == 'B' || matriz !! x !! y == 'R' = True
  | matriz !! x !! y /= ' ' && matriz !! x !! y /= ' ' = False
  | otherwise = nordeste (x - 1) (y + 1) matriz

--Verifica se há um bispo ou rainha na diagonal inferior direita
sudeste :: Int -> Int -> [String] -> Bool
sudeste x y matriz
  | x >= 8 || x < 0 || y >= 8 || y < 0 = False
  | matriz !! x !! y == 'B' || matriz !! x !! y == 'R' = True
  | matriz !! x !! y /= ' ' && matriz !! x !! y /= ' ' = False
  | otherwise = sudeste (x + 1) (y + 1) matriz

--Verifica se há um bispo ou rainha na diagonal inferior esquerda
sudoeste :: Int -> Int -> [String] -> Bool
sudoeste x y matriz
  | x >= 8 || x < 0 || y >= 8 || y < 0 = False
  | matriz !! x !! y == 'B' || matriz !! x !! y == 'R' = True
  | matriz !! x !! y /= ' ' && matriz !! x !! y /= ' ' = False
  | otherwise = sudoeste (x + 1) (y - 1) matriz

--Verifica se há um bispo ou rainha na diagonal superior esquerda
noroeste :: Int -> Int -> [String] -> Bool
noroeste x y matriz
  | x >= 8 || x < 0 || y >= 8 || y < 0 = False
  | matriz !! x !! y == 'B' || matriz !! x !! y == 'R' = True
  | matriz !! x !! y /= ' ' && matriz !! x !! y /= ' ' = False
  | otherwise = noroeste (x - 1) (y - 1) matriz

--Verifica se há bispo ou rainha nas diagonais
findDiagonal :: [Int] -> [String] -> Bool
findDiagonal (x:y:_) matriz = nordeste (x - 1) (y + 1) matriz || sudeste (x + 1) (y + 1) matriz || sudoeste (x + 1) (y - 1) matriz || noroeste (x - 1) (y - 1) matriz

--ENCONTRA CAVALO
--Verifica se há um cavalo numa determinada coordenada
encontraCavalo :: Int -> Int -> [String] -> Bool
encontraCavalo x y matriz
  | x >= 8 || x < 0 || y >= 8 || y < 0 = False
  | matriz !! x !! y == 'C' = True
  | otherwise = False

--Verifica se há um cavalo nas 8 possíveis posições
findHorse :: [Int] -> [String] -> Bool
findHorse (x:y:_) matriz = encontraCavalo (x - 2) (y + 1) matriz || encontraCavalo (x - 1) (y + 2) matriz || encontraCavalo (x + 1) (y + 2) matriz || encontraCavalo (x + 2) (y + 1) matriz || encontraCavalo (x + 2) (y - 1) matriz || encontraCavalo (x + 1) (y - 2) matriz || encontraCavalo (x - 1) (y - 2) matriz || encontraCavalo (x - 2) (y - 1) matriz


--ENCONTRA PEÃO
--Verifica se há um peão numa determinada coordenada
encontraPeao :: Int -> Int -> [String] -> Bool
encontraPeao x y matriz
  | x >= 8 || x < 0 || y >= 8 || y < 0 = False
  | matriz !! x !! y == 'P' = True
  | otherwise = False

--Verifica se há um peão nas 4 possíveis posições
findPeao :: [Int] -> [String] -> Bool
findPeao (x:y:_) matriz = encontraPeao (x - 1) (y + 1) matriz || encontraPeao (x + 1) (y + 1) matriz || encontraPeao (x + 1) (y - 1) matriz || encontraPeao (x - 1) (y - 1) matriz


--PEÇAS BRANCAS: minúsculas
--PEÇAS PRETAS: MAIÚSCULAS
main :: IO ()
main = do
    entrada <- getLine
    let lista = words entrada--["tcbdrbct","pppppppp","8","8","8","8","PPPPPPPP","TCBDRBCT"]
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
    
    --Verifica se há uma peça que possa dar xeque na diagonal
    else if findDiagonal whiteKingPos matriz then
      putStrLn $ "True"
      
    --Verifica se um Peão pode dar xeque
    else if findPeao whiteKingPos matriz then
      putStrLn $ "True"
      
    else
      putStrLn $ "False"
    
