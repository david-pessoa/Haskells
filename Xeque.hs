import Data.Char (ord)
let letras = "tcbdrpTCBDRP"
let numeros = "12345678"

member :: Char -> [Char] -> Bool
member _ [] = False
member y (a:x) = (y == a) || member y x

geraVetor :: Char -> [Char] -> [Char]
geraVetor _ [] = []
geraVetor (a:x)
  | (member a letras == True) = [a] ++ geraVetor x
  | otherwise = addVazio a ++ geraVetor









genMatriz :: [String] -> [String]
genMatriz (a:x)
  |





main :: IO ()
main = do
  putStrLn $ show (ord 'A')
