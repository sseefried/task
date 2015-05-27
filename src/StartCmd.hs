{-# LANGUAGE OverloadedStrings #-}
module StartCmd where

-- standard libraries

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf
import System.Exit
import Control.Monad
import Data.Time
import Data.Maybe (mapMaybe, isJust)
import Data.Either (either)

-- friends
import GetOpt
import Time
import Errors
import RecordSet
import ParseOpts

import Record (Record)
import qualified Record as R

--
-- | Flags for the "start" command
--
data StartCmdFlag =
    StartCmdTime     UTCTime
  | StartCmdKeyValue (Text, Text)

--
-- Given a UTCTime (to determine what day it is) and a TimeZone returns option descriptions
-- for the start command
--
startCmdOpts :: ZonedTime -> [OptDescr (Either String StartCmdFlag)]
startCmdOpts zt =
    [ Option "t" ["time"]      (OptArg timeToStartCmd "time") "start at time"
    , Option "k" ["key/value"] (OptArg keyValueToStartCmd "key/value") "key/value" ]
  where
    timeToStartCmd :: Maybe String -> Either String StartCmdFlag
    timeToStartCmd = maybe (Left "You have not provided a time argument.")
                           (either Left (Right . StartCmdTime) . parseTimeFlag zt)
    keyValueToStartCmd :: Maybe String -> Either String StartCmdFlag
    keyValueToStartCmd = maybe (Left "You have not provided a key/value argument.")
                               (either Left (Right . StartCmdKeyValue) . parseKeyValueFlag)

-- FIXME: Check that start time is after all things in the record set already.

startCmd :: ZonedTime -> [String] -> IO ()
startCmd zt args = do
  let (opts, nonOpts, errors) = getOptEither Permute (startCmdOpts zt) args
  exitWithErrorIf' (length errors > 0) (unlines errors)
  exitWithErrorIf (length nonOpts == 0) "You must provide a description for this task."
  exitWithErrorIf (length nonOpts /= 1) "There can only be one description for this task."
  start        <- getStartTime opts
  let keyValues = getKeyValues opts
  let descr = T.pack $ nonOpts !! 0
  rs <- readRecordSet
  exitWithErrorIf (isJust $ R.current rs) "There is already a current task. Run 'task clear'."
  -- Check if it overlaps with any previous tasks.
  when (not . R.null $ rs) $ do
    let lastFinish = R.recFinish . R.last $ rs
    exitWithErrorIf (lastFinish >= start)
    -- FIXME: Clean up
      (printf "Can't start a task at this time since the last record's finish time is at '%s'."
      (prettyTime (zonedTimeZone zt) lastFinish))
  writeCurrentRecord $ R.CurrentRecord descr start keyValues
  printf "Creating new task at '%s' with description '%s'.\n"
    (prettyTime (zonedTimeZone zt) start)
    (T.unpack descr)

getStartTime :: [StartCmdFlag] -> IO UTCTime
getStartTime flags
  | null utcTimes = getCurrentTime
  | otherwise     = return . head $ utcTimes
  where
    utcTimes = mapMaybe f flags
    f (StartCmdTime t) = Just t
    f _                 = Nothing

getKeyValues :: [StartCmdFlag] -> [(Text,Text)]
getKeyValues = mapMaybe f
  where
    f (StartCmdKeyValue p) = Just p
    f _                    = Nothing