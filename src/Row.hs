{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.ByteString.Lazy.Char8       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import System.Environment                         (getArgs)
import Text.Read                                  (readMaybe)

arg :: IO Int
arg = getArgs >>= \case
          [x] -> case readMaybe x of
                     Just n -> pure n
                     Nothing -> error "Not a number"
          _   -> error "Expected one integer argument"

main :: IO ()
main = L8.putStr =<< run <$> arg <*> L8.getContents

run :: Int -> ByteString -> ByteString
run n = head
      . drop n
      . L8.lines
