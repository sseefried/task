{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module QueryCmd where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Control.Monad
import           Text.Printf (printf)
import           Data.List (intersperse)
import           Text.Regex.Posix

-- friends
import           GetOpt
import           Time
import           Errors
import           RecordSet
import           ParseOpts
import           Record (Record, RecordSet)
import qualified Record as R

--
-- | Flags for the "start" command
--
data QueryCmdFlag =
    QueryCmdStartTime   UTCTime
  | QueryCmdFinishTime  UTCTime
  | QueryCmdKeyRegex    Text
  | QueryCmdValueRegex  Text

queryCmdOpts :: ZonedTime -> [OptDescr (Either String QueryCmdFlag)]
queryCmdOpts zt =
  [ Option "s" ["start-time"]
      (OptArg (timeToExportCmd  QueryCmdStartTime) "start time") "query from start time"
  , Option "f" ["finish-time"]
      (OptArg (timeToExportCmd QueryCmdFinishTime) "finish time") "query until finish time"
  , Option "k" ["key-matching"] (OptArg keyRegexToQueryCmd "key regex")
      "regular expression matching key"
  , Option "v" ["value-matching"] (OptArg valueRegexToQueryCmd "value regex")
      "regular expression match value"
  ]
  where
    timeToExportCmd :: (UTCTime -> QueryCmdFlag) -> Maybe String
                    -> Either String QueryCmdFlag
    timeToExportCmd f =
      maybe (Left "You have not provided a time argument.")
            (either Left (Right . f) . parseTimeFlag zt)
    keyRegexToQueryCmd :: Maybe String -> Either String QueryCmdFlag
    keyRegexToQueryCmd = maybe (Left "You have not provided a regular expression to match against a key")
                           (Right . QueryCmdKeyRegex . T.pack)
    valueRegexToQueryCmd :: Maybe String -> Either String QueryCmdFlag
    valueRegexToQueryCmd = maybe (Left $ "You have not provided a regular expression to "++
                                    "match against a value")
                            (Right . QueryCmdValueRegex . T.pack)

queryCmd :: ZonedTime -> [String] -> IO ()
queryCmd zt args = do
  let (flags, nonOpts, errors) = getOptEither Permute (queryCmdOpts zt) args
  exitWithErrorIf' (length errors > 0) (unlines errors)
  exitWithErrorIf (length nonOpts > 0) (printf "Unknown arguments: %s"
                                          (concat . intersperse ", " $ nonOpts))
  (startTime, finishTime) <- getStartAndFinishTime flags
  keyRegex                <- getKeyRegex flags
  valueRegex              <- getValueRegex flags
  rs                      <- readRecordSet
  let (t,recs) = totalInSeconds (startTime, finishTime) keyRegex valueRegex rs
  mapM_ (putStr . R.prettyRecord zt) recs

  printf "----\nTotal time: %s\n" (daysHoursMinutesSeconds t)

-- Order of flags is important. Later flags override earlier ones.
getStartAndFinishTime :: [QueryCmdFlag] -> IO (UTCTime, UTCTime)
getStartAndFinishTime flags = do
  initFinishTime <- getCurrentTime
  let initStartTime = addUTCTime (-100*365*86400) initFinishTime -- a century ago
      processFlag :: (UTCTime, UTCTime) -> QueryCmdFlag -> IO (UTCTime, UTCTime)
      processFlag sf@(s,f) flag = case flag of
        QueryCmdStartTime s'  -> return (s',f)
        QueryCmdFinishTime f' -> return (s, f')
        _                     -> return sf
  foldM processFlag (initStartTime, initFinishTime) flags

getKeyRegex :: [QueryCmdFlag] -> IO Text
getKeyRegex flags = foldM go ".*" flags
  where go _       (QueryCmdKeyRegex t) = return t
        go keyRegex _                   = return keyRegex

getValueRegex :: [QueryCmdFlag] -> IO Text
getValueRegex flags = foldM go ".*" flags
  where go _       (QueryCmdValueRegex t) = return t
        go keyRegex _                     = return keyRegex

------------

totalInSeconds :: (UTCTime, UTCTime) -> Text -> Text -> RecordSet -> (Integer, [Record])
totalInSeconds (s,f) keyRegex valueRegex rs =
  (foldl go 0 recs, recs)
  where
    recs = filter recMatches $  R.findBetween s f rs
    go :: Integer -> Record -> Integer
    go total r = total + R.duration r
    recMatches :: Record -> Bool
    recMatches r = any keyValueMatches (R.recKeyValues r)
    keyValueMatches :: (Text,Text) -> Bool
    keyValueMatches (k,v)  = encodeUtf8 k =~ encodeUtf8 keyRegex &&
                             encodeUtf8 v =~ encodeUtf8 valueRegex

