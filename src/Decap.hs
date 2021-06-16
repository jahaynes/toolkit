module Main where

import Data.ByteString.Lazy.Char8 (lines, interact, unlines)
import Prelude                    ((.), tail)
import System.IO                  (IO)

main :: IO ()
main = interact ( unlines
                . tail
                . lines )