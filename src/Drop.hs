{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.ByteString.Lazy.Char8       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Char                                  (isSpace)
import System.Environment                         (getArgs)
import Text.Read                                  (readMaybe)

arg :: IO Int
arg = getArgs >>= \case
          [x] -> case readMaybe x of
                     Just n -> pure n
                     Nothing -> error "Not a number"
          _   -> error "Expected one integer argument"

main :: IO ()
main = C8.putStr =<< run <$> arg <*> C8.getContents

run :: Int -> ByteString -> ByteString
run n = C8.unlines
      . map (lineDrop n)
      . C8.lines

lineDrop :: Int -> ByteString -> ByteString
lineDrop n = mconcat
           . drop (2 * n)
           . C8.groupBy (\a b -> isSpace a == isSpace b)
           . C8.dropWhile isSpace
