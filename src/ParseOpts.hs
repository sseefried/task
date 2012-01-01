module ParseOpts where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Char8
import Data.Aeson.Parser (jstring)
import Data.Aeson
import Control.Applicative
import Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.Zepto as Z
import Data.ByteString.Unsafe as B


import Text.Printf

import Data.Either

parseKeyValue :: String -> Either String (Text,Text)
parseKeyValue s = case parse parser . BS.pack  $ s of
  Done _ r   -> Right r
  Partial _  -> Left "Unexpected end of input on key/value pair"
  Fail s _ _ -> Left (printf "Error parsing key/value near: '%s'."
                     (BS.unpack . BS.takeWhile (not . (=='\n')) $ s))

  where
    parser :: Parser (Text,Text)
    parser = do
      key   <- jstring <* char ':'
      value <- jstring
      return (key,value)