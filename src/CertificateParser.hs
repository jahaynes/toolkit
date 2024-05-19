{-# LANGUAGE OverloadedStrings #-}

module CertificateParser ( parseCertificate ) where

import           Control.Arrow
import           Control.Monad.Combinators
import           Data.Aeson
import           Data.Aeson.Key (fromString)
import qualified Data.Aeson.KeyMap as KM
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec () Text

parseCertificate :: Text -> Value
parseCertificate txtCert =
    case parse certParser "certificateBundle" txtCert of
        Left e  -> error $ show e
        Right a -> a

certParser :: Parser Value
certParser = do
    (_, Validity va) <- manyTill_ keyValLine (try validity)
    Subject sub     <- subject
    pure . Object
         . KM.fromList
         $ [ ("subject", Object sub)
           , ("validity", Object va) ]

keyValLine :: Parser Chunk
keyValLine = do
    key <- hspace *> manyTill printChar (char ':')
    val <- hspace *> manyTill printChar eol
    pure $ KeyValLine (T.pack key) (T.pack val)

validity :: Parser Chunk
validity = do
    _ <- hspace *> string "Validity" *> eol
    KeyValLine "Not Before" notBefore <- keyValLine
    KeyValLine "Not After " notAfter  <- keyValLine
    pure . Validity
         . KM.fromList
         $ [ ("notBefore", String notBefore)
           , ("notAfter", String notAfter) ]

subject :: Parser Chunk
subject = do
    _ <- hspace *> string "Subject:" *> hspace
    keyVals <- sepEndBy section (string ", ")
    pure . Subject
         . KM.fromList
         . map (first fromString)
         . map (second (String . T.pack))
         $ keyVals

    where
    section = (,) <$> some alphaNumChar <* string " = "
                  <*> some (hexDigitChar <|> char '-')

data Chunk = KeyValLine Text Text
           | Validity !Object
           | Subject !Object
