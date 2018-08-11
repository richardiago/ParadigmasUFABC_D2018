module Huffman where

import Data.Char
import Data.List

--Criação do tipo arvore
data Arvore = Nulo | No Char Int String Arvore Arvore
            deriving Show

--recebe a string e retorna a tabela de frequências ordenada
contarFreq :: String -> [(Char, Int)]
contarFreq s = (sortOn snd [(y, count y s) | y <- nub s])

-- conta a ocorrência de um caracter em uma String
count :: Char -> String -> Int
count c s = length freq
  where
    freq = [y | y <- s, y == c]

--recebe a tabela de frequências e devolve uma lista de arvores
listTree :: [(Char, Int)] -> [Arvore]
listTree xs = [arv y | y <- xs]
  where
    arv ls = No (fst ls) (snd ls) "" Nulo Nulo

--reorganiza a lista de arvores
reorganiza_Arv :: Arvore -> [Arvore] -> [Arvore]
reorganiza_Arv (No c n a esq dir) []   = [(No c n a esq dir)]
reorganiza_Arv Nulo xs                 = xs
reorganiza_Arv (No c1 n1 a1 esq1 dir1) (No c2 n2 a2 esq2 dir2 : xs)
  | n1 > n2    = [(No c2 n2 a2 esq2 dir2)] ++ reorganiza_Arv (No c1 n1 a1 esq1 dir1) xs
  | otherwise  = [(No c1 n1 a1 esq1 dir1)] ++ ((No c2 n2 a2 esq2 dir2) : xs)

--recebe a lista de arvores e devolve a arvore completa
monta_Arv :: [Arvore] -> Arvore
monta_Arv []     = Nulo
monta_Arv (x:[]) = x
monta_Arv xs     = monta_Arv (reorganiza_Arv (monta (take 2 xs)) (drop 2 xs))
  where
     monta ((No c1 n1 a1 esq1 dir1) : (No c2 n2 a2 esq2 dir2): []) = No '☠' (n1 + n2) "" (No c1 n1 a1 esq1 dir1) (No c2 n2 a2 esq2 dir2)

  --atribui códigos para cada nó da árvore
atribuirCodigos :: Arvore -> [(Char, String)]
atribuirCodigos Nulo                                                       = []
atribuirCodigos (No c n a Nulo Nulo)                                       = [(c,a)]
atribuirCodigos (No c n a (No c1 n1 a1 esq1 dir1) (No c2 n2 a2 esq2 dir2)) = [(c,a)] ++ atribuirCodigos m ++ atribuirCodigos n
  where
    m = (No c1 n1 (a++"0") esq1 dir1)
    n = (No c2 n2 (a++"1") esq2 dir2)

--Filtra os nós com os caracteres que interessam
filtraCodigos :: [(Char, String)] -> [(Char, String)]
filtraCodigos xs = [y | y <- xs, fst y /= '☠']

--Retorna codigo do caracter c
retornaCodigo :: Char -> [(Char, String)] -> String
retornaCodigo _ []     = []
retornaCodigo c (x:xs)
  | c == fst x = snd x
  | otherwise  = retornaCodigo c xs

--codificar texto
codificar2 :: [(Char, String)] -> String -> String
codificar2 _ [] = []
codificar2 dic (x:xs) = retornaCodigo x dic ++ codificar2 dic xs

-- Recebe a String e devolve o dicionario com codigos
codificar1 :: String -> [(Char, String)]
codificar1 xs = filtraCodigos
              $ atribuirCodigos
              $ monta_Arv
              $ listTree
              $ contarFreq xs

--Devove a arvore binaria para decodificação
devolve_Arv :: String -> Arvore
devolve_Arv xs = monta_Arv
               $ listTree
               $ contarFreq xs

--Decodifica texto, recebe o texto a arvore e devolve o texto decodificado
--usado para verificar corretude
decodificar :: String -> Arvore -> Arvore -> String
decodificar [] raiz (No c n a esq dir) = [c]
decodificar (x:xs) raiz (No c n a esq dir)
  | c /= '☠'    = [c] ++ decodificar (x:xs) raiz raiz
  | x == '1'    = decodificar xs raiz dir
  | otherwise   = decodificar xs raiz esq

  --Conveter para decimal

--verifica se é multiplo de 8
lenMult8 :: String -> Int
lenMult8 s = (length s) `mod` 8

--adiciona zeros se necesário
addZeros :: String -> String
addZeros s | lenMult8 s == 0 = s
           | otherwise       = s ++ ['0' | i <- [1..lenMult8 s]]

--binario para decimal
binstr2dec :: String -> Int
binstr2dec x = aux x 0 ((length x)-1)
  where
    aux x n m
      | null x        = n
      | head x == '0' = aux (tail x) n (m-1)
      | otherwise     = aux (tail x) (n+ 2^m) (m-1)

--converte toda a String
convertBin :: String -> [Int]
convertBin s = converte (addZeros s)
  where
    converte xs
      | null xs = []
      | otherwise = [binstr2dec (take 8 xs)] ++ converte (drop 8 xs)
