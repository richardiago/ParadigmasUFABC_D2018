module Lzw where

import Data.List

--formar dicionario inicial
inicialDic:: String -> [(String, Int)]
inicialDic xs   = [y | y <- zip zs ns]
  where
    zs          = [toString g | g <- nub xs]
    ns          = [1..length xs]
    toString js = [ws | ws <- [js]]

--Retorna 0 se uma lista não contem string xs e caso
--contrario retona o numero correspondente do dicionario
contem :: String -> [(String, Int)] -> Int
contem _ []     = 0
contem xs (y:ys)
  | xs == fst y = snd y
  | otherwise   = contem xs ys

--adiciona um item (String, Int) ao fim do dicionario
addDic :: String -> [(String, Int)] -> [(String, Int)]
addDic st ys = ys ++ [(st, m)]
  where
    m = n + 1
    n = (snd (head (reverse ys)))


codifica :: String -> String -> [(String, Int)] -> [Int]
codifica [] x zs               = [contem x zs]
codifica (x:xs) ys zs
  | contem (ys++[x]) zs /= 0   = codifica xs (ys++[x]) zs
  | otherwise                  = [contem ys zs] ++ codifica xs [x] (addDic (ys++[x]) zs)
