{-# LANGUAGE OverloadedStrings #-}
module StartCmd where

-- standard libraries

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf
import System.Exit
import Control.Monad

-- friends
import GetOpt
import Time

--
-- | Flags for the "start" command
--
data StartCmdFlag =
  StartCmdTime LocalTime

--
-- Given a UTCTime (to determine what day it is) and a TimeZone returns option descriptions
-- for the start command
--
startCmdOpts :: TimeWithZone -> [OptDescr (Either String StartCmdFlag)]
startCmdOpts twz = [
    Option "t" ["time"] (OptArg timeToStartCmd "time") "start at time" ]
  where
    timeToStartCmd :: Maybe String -> Either String StartCmdFlag
    timeToStartCmd = maybe (Left "You have not provided a time argument") parseTheTime
      where
        parseTheTime :: String -> Either String StartCmdFlag
        parseTheTime s = case parseTaskTime twz s of
          Nothing      -> Left (printf "Could not parse time string `%s'" $ s)
          Just utcTime -> Right . StartCmdTime $ utcTime

startCmd :: TimeWithZone -> [String] -> IO ()
startCmd twz args = do
  let (opts, nonOpts, errors) = getOptEither Permute (startCmdOpts twz) args
  when (length errors > 0) $ do
    mapM_ putStrLn errors
    exitWith (ExitFailure 1)
