{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import           Control.Concurrent.STM (atomically)
import           Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.ByteString.Base64 as B64
import qualified Data.Vector as V
import           System.IO (hClose)
import           System.Process.Typed

main :: IO ()
main = do

    content <- L8.getContents
    let Right root = eitherDecode content :: Either String Value
    L8.putStrLn . encode $ root
    L8.putStrLn . encode =<< walk operation root

operation :: Maybe Key -> Text -> IO (Maybe Value)
operation = steps [unpackCert, decodeDataObj]

-- Modifications
steps :: [a -> b -> IO (Maybe c)] -> a -> b -> IO (Maybe c)
steps     []  _ _ = pure Nothing
steps (f:fs) mk t =
    f mk t >>= \case
        Just t' -> pure $ Just t'
        Nothing -> steps fs mk t

decodeDataObj :: Maybe Key -> Text -> IO (Maybe Value)
decodeDataObj (Just "data") t =
    case eitherDecode (L8.fromStrict $ encodeUtf8 t) of
        Left l    -> error l
        Right val -> Just <$> walk operation val
decodeDataObj _ _ = pure Nothing

unpackCert :: Maybe Key -> Text -> IO (Maybe Value)
unpackCert (Just "certificateBundle") cert = do
    let procConfig = setStdin createPipe
                   . setStdout byteStringOutput
                   $ shell "openssl x509 -inform PEM -in /dev/stdin -noout -text" 
    decodedCert <- withProcessWait_ procConfig $ \p -> do
                       C8.hPutStr (getStdin p) (b64Decode' cert)
                       hClose     (getStdin p)
                       atomically (getStdout p)
    case decodeUtf8' (L8.toStrict decodedCert) of
        Left l -> error $ unlines ["Could not utf8 encode cert output", show l]
        Right t -> pure . Just . String $ t
unpackCert                          _ _ = pure Nothing

b64Decode' :: Text -> C8.ByteString
b64Decode' t =
    case B64.decodeBase64Untyped (encodeUtf8 t) of
        Left l -> error $ unlines ["Could not base64 decode", T.unpack l]
        Right unb64 -> unb64
    

-- Traversals

walk :: (Maybe Key -> Text -> IO (Maybe Value)) -> Value -> IO Value
walk f v@(String t)  = fromMaybe v <$> f Nothing t
walk f   (Array xs)  = Array  <$> V.mapM (walk f) xs
walk f   (Object km) = Object <$> walkObj f km
walk _ v             = pure v

walkObj :: (Maybe Key -> Text -> IO (Maybe Value)) -> Object -> IO Object
walkObj f = KM.traverseWithKey (walkKeyVal f)

walkKeyVal :: (Maybe Key -> Text -> IO (Maybe Value)) -> Key -> Value -> IO Value
walkKeyVal f k v@(String t) = fromMaybe v <$> f (Just k) t
walkKeyVal f _ v            = walk f v