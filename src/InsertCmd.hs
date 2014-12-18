{-# LANGUAGE OverloadedStrings #-}
module InsertCmd (
  insertCmd, insertCmdOpts, insertCmdUsage) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf
-- import System.Exit
-- import Control.Monad
import Data.Time
import Data.Maybe (mapMaybe, isJust, fromJust)
-- import Data.Either (either)
--
-- -- friends
import GetOpt
import Time
import Errors
import RecordSet
import ParseOpts
--
import Record (Record)
import qualified Record as R
import qualified RecordSet as R


data InsertCmdFlag =
    InsertCmdKeyValue (Text, Text)


insertCmdUsage :: String -> String
insertCmdUsage name = printf "Usage: %s <start time> <finish time> <description>" name

insertCmdOpts :: ZonedTime -> [OptDescr (Either String InsertCmdFlag)]
insertCmdOpts zt =
   [ Option "k" ["key-value"] (OptArg keyValueToInsertCmd "key/value") "key/value" ]
  where
    keyValueToInsertCmd :: Maybe String -> Either String InsertCmdFlag
    keyValueToInsertCmd = maybe (Left "You have not provided a key/value argument.")
                               (either Left (Right . InsertCmdKeyValue) . parseKeyValueFlag)

insertCmd :: ZonedTime -> [String] -> IO ()
insertCmd zt args = do
  let (opts, nonOpts, errors) = getOptEither Permute (insertCmdOpts zt) args
  let keyValues = getKeyValues opts
  exitWithErrorIf' (length errors > 0) (unlines errors)
  exitWithErrorIf (length nonOpts /= 3) (insertCmdUsage "task")
  let [startStr, finishStr, descr] = nonOpts
  start  <- parseTimeOrExit zt "start time: "  startStr
  finish <- parseTimeOrExit zt "finish time: " finishStr
  rs <- R.readRecordSet
  r  <- R.newRecord rs (T.pack descr) start finish keyValues
  case R.insert r rs of
    Left error -> exitWithError $ "This task overlaps with another! "
    Right rs' -> R.writeRecordSet rs'
  printf "Task inserted"

parseTimeOrExit :: ZonedTime -> String -> String -> IO UTCTime
parseTimeOrExit zt prefix s = do
  case parseTimeFlag zt s of
    Left  error   -> exitWithError (prefix ++ error)
    Right utcTime -> return utcTime

getKeyValues :: [InsertCmdFlag] -> [(Text,Text)]
getKeyValues = mapMaybe f
  where
    f (InsertCmdKeyValue p) = Just p
    f _                     = Nothing

