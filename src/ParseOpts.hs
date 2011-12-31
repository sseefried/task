module ParseOpts where

import Data.Text (Text)
import qualified Data.Text as T

import Text.Printf

import Data.Either

-- FIXME
parseKeyValue :: String -> Either String (Text,Text)
parseKeyValue s = Left (printf "Cannot parse key/value '%s'" s)

-- parseKeyValues :: [String] -> Either String [(Text, Text)]
-- parseKeyValues ss
--   | null errors = Right kvs
--   | otherwise = Left . concat $ errors
--   where
--     (errors, kvs) = partitionEithers . map parseKeyValue $ ss