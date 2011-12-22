{-# LANGUAGE OverloadedStrings, PatternGuards #-}
--
-- Author: Sean Seefried
-- Date:   Wed 21 Dec 2011
--
module Main where

-- library imports
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import System.Locale (defaultTimeLocale)
import Data.Maybe
import Text.Printf

-- friends
import System.Console.GetOpt


--
-- | Flags for the "start" command
--
data StartCmdFlag =
  StartCmdTime  LocalTime
--
-- | Flags for the "modify" command
--
data ModifyCmdFlag =
    ModifyCmdId     Text
  | ModifyCmdStart  LocalTime
  | ModifyCmdFinish LocalTime


data CurrentTime = CurrentTime UTCTime TimeZone

--
-- Given a UTCTime (to determine what day it is) and a TimeZone returns option descriptions
-- for the start command
--
startCmdOpts :: CurrentTime -> [OptDescr (Either Text StartCmdFlag)]
startCmdOpts ct = [
    Option "t" ["time"] (OptArg timeToStartCmd "time") "start at time"
  ]
  where
    timeToStartCmd :: Maybe String -> Either Text StartCmdFlag
    timeToStartCmd = maybe (Left "No time argument") parseTheTime
      where
        parseTheTime :: String -> Either Text StartCmdFlag
        parseTheTime s = case parseTaskTime ct s of
          Nothing      -> Left (T.pack . printf "Could not parse time `%s'" $ s)
          Just utcTime -> Right . StartCmdTime $ utcTime

--
-- Parse the task time, trying a variety of different formats.
--
parseTaskTime :: CurrentTime -> String -> Maybe LocalTime
parseTaskTime (CurrentTime t tz) s
  | Just lt <- p "%Y-%m-%d %l:%M" full = Just lt
  | Just lt <- p "%Y-%m-%d %l:%M" s    = Just lt
  | Just lt <- p "%e %b %Y %l:%M" s    = Just lt
  | Just lt <- p "%e %B %Y %l:%M" s    = Just lt
  | Just lt <- p "%e %M %Y %l:%M" s    = Just lt
  | otherwise                          = Nothing

  where
    p fmt = parseTime defaultTimeLocale fmt
    full  = formatTime defaultTimeLocale "%Y-%m-%d" t ++ " " ++ s


-- modifyCmdOpts :: [OptDescr ModifyCmdFlag]
-- modifyCmdOpts = [
--   Option "i" ["id"] (OptArg idToStardCmd "stringy") "id of entry"
--   ]

-- commands :: [(Text, [OptDescr Text])]
-- commands =
--   [ ("start",
--      [ Option "i" ["id"] (OptArg ) ()])
--   , ("finish", [])
--   , ("modify", [])
--   , ("query",  [])
--   , ("export", [])
--   ]

main :: IO ()
main = do
  ct <- mkCurrentTime
  putStr $ usageInfo "Header" (startCmdOpts ct)

mkCurrentTime :: IO CurrentTime
mkCurrentTime = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ CurrentTime t tz

test :: IO ()
test = do
  ct@(CurrentTime _ tz) <- mkCurrentTime
  let mtime = parseTaskTime ct "00:23"
  case mtime of
    Just time -> printf "Local: %s\nUTC: %s\n" (show time) (show $ localTimeToUTC tz time)
    Nothing    -> return ()
