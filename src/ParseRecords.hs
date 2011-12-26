{-# LANGUAGE OverloadedStrings #-}
--
-- Author: Sean Seefried
-- Date:   Wed 21 Dec 2011
--

--
-- | Defines data type and function for the 'Record' data type.
--
module ParseRecords where

-- standard libraries
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Aeson.Encode (fromValue)
import Blaze.ByteString.Builder (toByteString)
import Data.Time
import System.Locale (defaultTimeLocale)
import Control.Monad (mzero)
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec as AP
import Data.Map (Map)
import qualified Data.Map as M
import Control.Arrow
import Data.Either
import Text.Printf
import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Data.Maybe -- FIXME: Debug


-- friends
import Record (Record(..), RecordSet)
import qualified Record as R

instance FromJSON Record where
  parseJSON (Object v) =
        Record <$> (v .: "id")
               <*> (v .: "start"      >>= parseJSON)
               <*> (v .: "finish"     >>= parseJSON)
               <*> (v .: "key_values" >>= parseJSON)
  parseJSON _ = mzero

instance ToJSON Record where
  toJSON (Record id start finish keyValues) =
    object ["id" .= id, "start" .= start, "finish" .= finish, "key_values" .= keyValues]

--
-- Parse a record and return either the record or an error and the remainder of the input (if any)
--
parseRecord :: BS.ByteString -> (Either String Record, BS.ByteString)
parseRecord bs =
  case AP.parse json bs of
    AP.Done rest r -> case parseEither parseJSON r of
      Left err -> (Left err,            rest)
      Right r  -> (R.validateRecord r,  rest)
    AP.Partial _   -> (Left "Unexpected end of input", "")
    AP.Fail s _ _  -> (Left (printf "Error near: '%s'" (BS.unpack s)), "")


parseRecords :: BS.ByteString -> Either [String] RecordSet
parseRecords str = go 1 str [] R.empty
  where
    go :: Int -> BS.ByteString -> [String] -> RecordSet -> Either [String] RecordSet
    go lineNumber s errs rs
      | BS.all whiteSpace s = if null errs then Right rs else Left (reverse errs)
      | otherwise =
        case result of
          Left err -> go (lineNumber+1) s' (addLineNumber err:errs) rs
          Right r  -> case rs `R.add` r of
            Left  err -> go (lineNumber+1) s' (addLineNumber err:errs) rs
            Right rs' -> go (lineNumber+1) s' errs rs'
        where
          (result, s') = parseRecord s
          whiteSpace c = c `elem` "\n\r\t\f "
          addLineNumber err = printf "%d:%s" lineNumber err

parseRecordFile :: FilePath -> IO (Either [String] RecordSet)
parseRecordFile path = BS.readFile path >>= (return . parseRecords)