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
import Record

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

parseRecord :: BS.ByteString -> Either String Record
parseRecord bs =
  case AP.parse json bs of
    AP.Done rest r -> parseEither parseJSON r :: Either String Record
    AP.Partial _   -> Left "Unexpected end of input"
    AP.Fail s _ _  -> Left (printf "Error near: '%s'" (BS.unpack s))

parseRecords :: BS.ByteString -> Either [String] (Map UTCTime Record)
parseRecords s = if not (null errs) then Left errs else Right (toMap records)
  where
    (errs, records) = partitionEithers . addLineNumbers . map (second parseRecord) $ lines
    lines :: [(Int, BS.ByteString)]
    lines = zip [1..] (BS.splitWith (=='\n') s)
    toMap :: [Record] -> Map UTCTime Record
    toMap = foldl ins M.empty
    ins m r = M.insert (recStart r) r m

addLineNumbers :: [(Int, Either String Record)] -> [Either String Record]
addLineNumbers = map f
  where
    f :: (Int, Either String Record) -> Either String Record
    f (i, eitherRecord) = case eitherRecord of
      Left err -> Left (printf "%d: %s" i err)
      Right r  -> Right r

parseRecordFile :: FilePath -> IO (Either [String] (Map UTCTime Record))
parseRecordFile path = BS.readFile path >>= (return . parseRecords)
