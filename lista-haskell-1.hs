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
len :: [Integer] -> Integer
len [] = 0
len (a:x) = 1 + len x

-- k, count, lista, soma do subvetor
soma :: Integer -> Integer -> [Integer] -> Integer
soma _ _ [] = 0
soma k count list = 



totk :: Integer -> [Integer] -> [Integer]
totk _ [] = []
totk a list = 



main :: IO ()
main = do
  putStrLn $ show (del_rep [ 3 , 1 , 2 , 1 , 2 , 3 ])
  
  
