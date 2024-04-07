------------------mergeSort------------------------
merge [] [] = []
merge lst [] = lst
merge [] lst = lst
merge (a:lsA) (b:lsB)
    | a < b = a:merge lsA (b:lsB)
    | otherwise = b:merge (a:lsA) lsB
    
splitaux [] ls1 ls2 = (ls1, ls2)
splitaux [a] ls1 ls2 = (a:ls1, ls2)
splitaux (a:b:ls) ls1 ls2 = splitaux ls (a:ls1) (b:ls2)

split [] = ([], [])
split [a] = ([a], [])
split [a,b] = ([a], [b])
split lst = splitaux lst [] []


mergeSort [] = []
mergeSort [a] = [a]
mergeSort lst = merge (mergeSort l1) (mergeSort l2)
    where (l1, l2) = split lst

------------------RLE-------------------------------
rleaux [] sym count = [(sym, count)]
rleaux (a:ls) sym count
    | a == sym = rleaux ls sym (count + 1)
    | otherwise = (sym, count): rleaux ls a 1

rle [] = []
rle (a:ls) = rleaux ls a 1

--------------------RLD----------------------------
rpt char 0 = []
rpt char num = char:rpt char (num - 1)

rld [] = []
rld ((char, num):ls) = rpt char num ++ rld ls

------------------QuickSort (sem partition)-------------------------
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (p:xs) = quickSort lesser ++ [p] ++ quickSort greater
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

-----------------------Árvore em Haskell----------------------------
data Tree = N Int Tree Tree | F Int --Criando estrutura de dados Tree
  deriving (Show, Eq)

-- Criando uma árvore
t1 = N 1
         (N 2 (N 3 (F 4) (F 4)) (F 3))
         (N 2 (F 3) (F 3))

-- Pré Ordem
pre (F n) = [n]
pre (N n te td) = [n] ++ pre te ++ pre td

-- Em ordem
ino (F n) = [n]
ino (N n te td) = ino te ++ [n] ++ ino td

-- Pós Ordem
pos (F n) = [n]
pos (N n te td) = pos te ++ pos td ++ [n]

-- Substituindo todos os elementos a por b na árvore
subs (F n) a b
    | n == a = (F b)
    | otherwise = (F n)
subs (N n te td) a b
    | n == a = (N b (subs te a b) (subs td a b))
    | otherwise = (N n (subs te a b) (subs td a b))

-- Calcula altura da árvore
altura (F _) = 1
altura (N _ te td)
    | alturaEsq > alturaDir = alturaEsq + 1
    | otherwise = alturaDir + 1
    where
        alturaEsq = altura te
        alturaDir = altura td

-- Quantidade de elementos na árvore
qtde (F _) = 1
qtde (N _ te td) = 1 + qtde te + qtde td

--Substitui todos os elemento da árvore pelo número de ocorrências do mesmo na árvore
qtos tree = qtaux tree tree

qtaux (F n) tree = (F (qtd n tree))
qtaux (N n te td) tree = (N (qtd n tree) (qtaux te tree) (qtaux td tree))

--Calcula número de ocorrências de um elemento na árvore
qtd elt (F n) | elt==n = 1 
              | otherwise = 0
qtd elt (N n te td) | elt == n = 1 + (qtd elt te) + (qtd elt td)
                    | otherwise = (qtd elt te) + (qtd elt td)

