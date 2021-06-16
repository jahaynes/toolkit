module Main where

import Data.ByteString.Lazy.Char8 (lines, interact, unlines, unwords, words)
import Prelude                    ((.), map, reverse)
import System.IO                  (IO)

main :: IO ()
main = interact ( unlines
                . map (unwords . reverse . words)
                . lines )
