{-# LANGUAGE OverloadedStrings #-}
--
-- Author: Sean Seefried
-- Date:   Wed 21 Dec 2011
--

--
-- | Defines data type and function for the 'Record' data type.
--
module Record where

-- standard libraries
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
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

import Data.Maybe -- FIXME: Debug

data Record = Record { recId     :: Text
                     , recStart  :: UTCTime
                     , recFinish :: UTCTime
                     , recKeyValues :: [ (Text, Text) ] } deriving Show

instance FromJSON Record where
  parseJSON (Object v) =
        Record <$> (v .: "id")
               <*> (v .: "start"  >>= parseJSON)
               <*> (v .: "finish" >>= parseJSON)
               <*> (v .: "key_values" >>= parseJSON)
  parseJSON _ = mzero

instance ToJSON Record where
  toJSON (Record id start finish keyValues) =
    object ["id" .= id, "start" .= start, "finish" .= finish, "key_values" .= keyValues]

parseRecord :: BS.ByteString -> Maybe Record
parseRecord bs =
  case AP.parse json bs of
    AP.Done rest r -> parseMaybe parseJSON r :: Maybe Record
    _              -> Nothing

parser :: BS.ByteString -> [Record]
parser = catMaybes . map parseRecord . BS.splitWith (=='\n')

parseRecords :: BS.ByteString -> Map Text Record
parseRecords = toMap . parser
  where
    toMap :: [Record] -> Map Text Record
    toMap = foldl ins M.empty
    ins m r = M.insert (recId r) r m

parseRecordFile :: FilePath -> IO (Map Text Record)
parseRecordFile path = BS.readFile path >>= (return . parseRecords)

---------
-- Tests

testUTCTime :: UTCTime
testUTCTime = fromJust $ parseTime defaultTimeLocale "%Y-%m-%d %H:%M" "2012-12-12 11:32"

testRecord :: Record
testRecord = Record "unique" testUTCTime testUTCTime []

testRecordStr ::  BS.ByteString
testRecordStr = toByteString . fromValue . toJSON $ testRecord
