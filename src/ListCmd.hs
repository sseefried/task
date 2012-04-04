{-# LANGUAGE OverloadedStrings #-}
module ListCmd (
  listCmdOpts,
  listCmd

) where


-- standard libraries
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf
import System.Exit
import Control.Monad
import Data.Time
import Data.Maybe (mapMaybe, isJust)
import Data.Either (either)
import Data.List (intersperse)

-- friends
import GetOpt
import Time
import Errors
import RecordSet
import ParseOpts
import StringUtils

import Record (Record)
import qualified Record as R

--
-- | Flags for the "start" command
--
data ListCmdFlag =
     ListCmdAfterTime   UTCTime
   | ListCmdBeforeTime  UTCTime
   -- limit on number of records to show. 'Nothing' means it was not numeric.
   | ListCmdLimit       Int

--
-- Given a UTCTime (to determine what day it is) and a TimeZone, returns option descriptions
-- for the start command.
--
listCmdOpts :: ZonedTime -> [OptDescr (Either String ListCmdFlag)]
listCmdOpts zt =
    [ Option "a" ["after"]  (OptArg (timeToListCmd ListCmdAfterTime) "time")
            "list tasks after time"
    , Option "b" ["before"] (OptArg (timeToListCmd ListCmdBeforeTime) "time")
             "list tasks before time"
    , Option "l" ["limit"]  (OptArg limitToListCmd "number")
             "limit list to n tasks (default 10)" ]
  where
    limitToListCmd :: Maybe String -> Either String ListCmdFlag
    limitToListCmd = maybe (Left "You have not provided a numerical argument")
                              (maybe (Left "Limit is not a number") (Right . ListCmdLimit) .
                              maybeRead)
    timeToListCmd :: (UTCTime -> ListCmdFlag) -> Maybe String -> Either String ListCmdFlag
    timeToListCmd f = maybe (Left "You have not provided a time argument.")
                            (either Left (Right . f) . parseTimeFlag zt)

listCmd :: ZonedTime -> [String] -> IO ()
listCmd zt args = do
  let (opts, nonOpts, errors) = getOptEither Permute (listCmdOpts zt) args
  exitWithErrorIf' (length errors > 0) (unlines errors)
  limit <- getLimit opts
  rs    <- readRecordSet
  let records = R.lastN limit rs
  mapM_ (prettyRecord zt) records


prettyRecord :: ZonedTime -> Record -> IO ()
prettyRecord zt r = printf "%s - %s: %s (%s)\n"
                      (pt R.recStart)
                      (pt R.recFinish)
                      (T.unpack . R.recDescr $ r)
                      (T.unpack . T.concat . intersperse ", " . map join $ R.recKeyValues r)
  where
    pt f = prettyTime (zonedTimeZone zt) (f r)
    join (t,t') = t `T.append` ":" `T.append` t'

getLimit :: [ListCmdFlag] -> IO Int
getLimit flags
  | null limits = return 10 -- default is 10
  | otherwise   = return . head $ limits
  where
    limits = mapMaybe f flags
    f (ListCmdLimit n) = Just n
    f _                = Nothing