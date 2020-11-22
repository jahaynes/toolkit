module Main where

import Data.ByteString.Lazy.Char8 (getContents, lines, putStr, unlines)
import Prelude                    ((=<<), (.), tail)
import System.IO                  (IO)

main :: IO ()
main = putStr
     . unlines
     . tail
     . lines
   =<< getContents