import Data.Char (ord)
let letras = "tcbdrpTCBDRP"
let numeros = "12345678"

member :: Char -> [Char] -> Bool
member _ [] = False
member y (a:x) = (y == a) || member y x

addVazioInt :: Int -> [Char]
addVazioInt num
  | num <= 0 = []
  | otherwise replicate num ' '

addVazio :: Char -> [Char]
addVazio num = addVazioInt(ord num)

geraVetor :: Char -> [Char] -> [Char]
geraVetor _ [] = []
geraVetor (a:x)
  | (member a letras == True) = [a] ++ geraVetor x
  | otherwise = addVazio a ++ geraVetor x

genMatriz :: [String] -> [String]
genMatriz (a:x) = geraVetor(take a) a ++ geraMatriz x





main :: IO ()
main = do
  putStrLn $ show (genMatriz ["tcbdrbct","pppppppp","8","8","8","8","PPPPPPPP","TCBDRBCT"])
