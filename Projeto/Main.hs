module Main where

import Huffman
import Lzw
import Data.Word
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BStrict
import System.Environment

int2w16 :: [Int] -> [Word16]
int2w16 xs = map fromIntegral xs


main :: IO ()
main =    do args <- getArgs
             s <- readFile (head args)

             --Huffman
             let hdic = codificar1 s
             let hcod = codificar2 hdic s
             let a = convertBin hcod
             let hbin = runPut (mapM_ putWord16le (int2w16 a))

             BS.writeFile "codificado_huffman.bin" hbin

             --LZW
             let ldic = inicialDic s
             let lcod = codifica s "" ldic
             let lbin = runPut (mapM_ putWord16le (int2w16 lcod))

             BS.writeFile "codificado_lzw.bin" lbin
