{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.ByteString.Base64 as B64
import qualified Data.Vector as V

main :: IO ()
main = do

    content <- L8.getContents

    let x = eitherDecode content :: Either String Value

    let y = case x of
                Left _  -> error "Couldn't decode"
                Right v -> visit (== "bar") interpret v

    L8.putStrLn $ encode y

visit :: (Key -> Bool) -> (Text -> Either String Text) -> Value -> Value
visit p f (Array xs)  = Array (V.map (visit p f) xs)
visit p f (Object km) = Object (KM.mapWithKey (visitKeyVal p f) km)
visit _ _ v           = v

visitKeyVal :: (Key -> Bool) -> (Text -> Either String Text) -> Key -> Value -> Value
visitKeyVal p f k (String v) | p k =
    case f v of
        Left e  -> error $ unlines ["Error handling key-val pair:", show (k, v), e]
        Right r -> String r
visitKeyVal _ _ _ v = v

interpret :: Text -> Either String Text
interpret t =

    case B64.decodeBase64Untyped (encodeUtf8 t) of
        Right unb64 -> case decodeUtf8' unb64 of
                           Left {} -> Left "Base64-decoded string, but could not reencode into utf8"
                           Right utf8 -> Right utf8
        Left l -> Left $ T.unpack l
