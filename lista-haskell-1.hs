module Main where

--Exercício 1
tot3 :: [Integer] -> [Integer]
tot3 [] = []
tot3 [a] = [a]
tot3 [a,b] = [a + b]
tot3 (a:b:c:x) = [a + b + c] ++ tot3 x

--Exercício 2
rev :: [Integer] -> [Integer]
rev [] = []
rev [a] = [a]
rev [a,b] = [b,a]
rev (a:x) = (rev x) ++ [a]

--Exercício 3
seg :: String -> Char
seg (_:b:_) = b

--Exercício 4
member :: Integer -> [Integer] -> Bool
member _ [] = False
member y (a:x) = (y == a) || member y x


del_rep :: [Integer] -> [Integer]
del_rep [] = []
del_rep [a] = [a]
del_rep (a:x)
  | member a x = del_rep x
  | otherwise = [a] ++ del_rep x
  
--Exercício 5
aux :: Int -> [Int] -> [Int]
aux _ [] = []
aux k list = sum (take k list) : aux k (drop k list)

totk :: Int -> [Int] -> [Int]
totk k list
  | k <= 0 = error "k deve ser maior que 0"
  | otherwise = aux k list
  
--Exercício 6
trok2 :: [Integer] -> [Integer]
trok2 [] = []
trok2 [a] = [a]
trok2 [a, b] = [b, a]
trok2 (a:b:x) = b:a:trok2 x

--Exercício 7
aux2 :: Int -> String -> String
aux2 _ [] = []
aux2 k string = take (k - 1) string ++ aux2 k (drop k string)

delk :: Int -> String -> String
delk k string
  | k <= 1 = error "k deve ser maior que 1"
  | otherwise = aux2 k string

main :: IO ()
main = do
  putStrLn $ show (delk 3 "anonimamentes")
  
  
