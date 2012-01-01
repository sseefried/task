{-# LANGUAGE OverloadedStrings #-}
module FinishCmd (
  finishCmd, finishCmdOpts
  ) where

-- standard libraries

-- import Data.Text (Text)
-- import qualified Data.Text as T
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

--
-- | Flags for the "start" command
--
data FinishCmdFlag =
    FinishCmdTime UTCTime


--
-- Given a UTCTime (to determine what day it is) and a TimeZone returns option descriptions
-- for the start command
--
finishCmdOpts :: ZonedTime -> [OptDescr (Either String FinishCmdFlag)]
finishCmdOpts zt =
    [ Option "t" ["time"] (OptArg timeToFinishCmd "time") "finish at time" ]
  where
    timeToFinishCmd :: Maybe String -> Either String FinishCmdFlag
    timeToFinishCmd = maybe (Left "You have not provided a time argument.")
                            (either Left (Right . FinishCmdTime) . parseTimeFlag zt)

finishCmd :: ZonedTime -> [String] -> IO ()
finishCmd zt args = do
  let (opts, nonOpts, errors) = getOptEither Permute (finishCmdOpts zt) args
  exitWithErrorIf (length nonOpts > 0) (printf "Unknown parameters '%s'" (show nonOpts))
  rs <- readRecordSet
  exitWithErrorIf (not . R.isCurrent $ rs) "There is no current task. Run 'task start'."
  let current = fromJust . R.current $ rs
  finish        <- getFinishTime opts
  exitWithErrorIf (R.crecStart current >= finish)
                  (printf "Finish time is the same as or before start time")
  rs' <- R.finishCurrent rs finish
  writeRecordSet rs'
  printf "Finishing current task at '%s'\n" (prettyTime finish (zonedTimeZone zt))



getFinishTime :: [FinishCmdFlag] -> IO UTCTime
getFinishTime flags
  | null utcTimes = getCurrentTime
  | otherwise     = return . head $ utcTimes
  where
    utcTimes            = mapMaybe f flags
    f (FinishCmdTime t) = Just t

