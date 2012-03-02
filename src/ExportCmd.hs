{-# LANGUAGE OverloadedStrings #-}
module ExportCmd where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Text.Printf
import System.Exit
import Control.Monad
import Data.Time
import Data.Maybe (mapMaybe, isJust)
import Data.Either (either)
import Data.CSV.Enumerator 
import qualified Data.ByteString.Char8 as BS

-- friends
import GetOpt
import Time
import Errors
import RecordSet
import ParseOpts

import Record (Record, RecordSet)
import qualified Record as R

--
-- | Flags for the "start" command
--
data ExportCmdFlag =
    ExportCmdStartTime   UTCTime
  | ExportCmdFinishTime  UTCTime
  | ExportCmdKey    Text -- exporting rows just for this key

--
-- Given a UTCTime (to determine what day it is) and a TimeZone returns option descriptions
-- for the start command
--
exportCmdOpts :: ZonedTime -> [OptDescr (Either String ExportCmdFlag)]
exportCmdOpts zt =
    [ Option "s" ["start-time"]
      (OptArg (timeToExportCmd  ExportCmdStartTime) "start time") "export from start time",
      Option "f" ["finish-time"]
      (OptArg (timeToExportCmd ExportCmdFinishTime) "finish time") "export until finish time"
    , Option "k" ["key-value"] (OptArg keyToExportCmd "key") "key" ]
  where
    timeToExportCmd :: (UTCTime -> ExportCmdFlag) -> Maybe String
                    -> Either String ExportCmdFlag
    timeToExportCmd f =
      maybe (Left "You have not provided a time argument.")
            (either Left (Right . f) . parseTimeFlag zt)
    keyToExportCmd :: Maybe String -> Either String ExportCmdFlag
    keyToExportCmd = maybe (Left "You have not provided a key/value argument.")
                           (Right . ExportCmdKey . T.pack)

exportCmd :: ZonedTime -> [String] -> IO ()
exportCmd zt args = do
  let (opts, nonOpts, errors) = getOptEither Permute (exportCmdOpts zt) args
  exitWithErrorIf (null nonOpts) "You have not provided a filename"
  exitWithErrorIf (length nonOpts /= 1) "Please provide only one filename"
  let [ filename ] = nonOpts
  rs <- readRecordSet
  finishTime <- getCurrentTime
  let startTime = addUTCTime (-100*365*86400) finishTime -- a century ago
  exportRecordsToCSV filename (zonedTimeZone zt) startTime finishTime rs
  return ()

exportRecordsToCSV :: String -> TimeZone -> UTCTime -> UTCTime -> RecordSet -> IO ()
exportRecordsToCSV filename tz startTime finishTime rs = do
  writeCSVFile defCSVSettings filename
    (map (recordToRow tz) $ R.findBetween startTime finishTime rs)
  return ()

recordToRow :: TimeZone -> Record -> Row
recordToRow tz r = [ (g . R.recStart  $ r),
                     (g . R.recFinish $ r),
                     (f . R.recDescr  $ r) ] ++ map h (R.recKeyValues r)
  where
    f = encodeUtf8
    g = BS.pack . isoTime tz
    h (key,value) = encodeUtf8 key `BS.append` ":" `BS.append` encodeUtf8 value