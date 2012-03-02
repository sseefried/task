{-# LANGUAGE PatternGuards #-}
module Time (
  module Data.Time,
  module Time) where

-- standard libraries
import Data.Time
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import Text.Printf

--
-- Parse the task time, trying a variety of different formats.
-- Parses the time in local time but converts it to UTC time.
--
parseTaskTime :: ZonedTime -> String -> Maybe UTCTime
parseTaskTime zt s
  | Just lt <- p "%Y-%m-%d %l:%M" full    = Just lt
  | Just lt <- p "%Y-%m-%d %l:%M:%S" full = Just lt

  | Just lt <- p "%Y-%m-%d %l:%M" s       = Just lt
  | Just lt <- p "%Y-%m-%d %l:%M:%S" s    = Just lt

  | Just lt <- p "%e %b %Y %l:%M" s       = Just lt
  | Just lt <- p "%e %b %Y %l:%M:%S" s    = Just lt

  | Just lt <- p "%e %B %Y %l:%M" s       = Just lt
  | Just lt <- p "%e %B %Y %l:%M:%S" s    = Just lt

  | Just lt <- p "%e %M %Y %l:%M" s       = Just lt
  | Just lt <- p "%e %M %Y %l:%M:%S" s    = Just lt
  | otherwise                             = Nothing

  where
    tz      = zonedTimeZone zt
    p fmt s = fmap (localTimeToUTC tz) (parseTime defaultTimeLocale fmt s)
    full    = printf "%s %s" (formatTime defaultTimeLocale "%Y-%m-%d" $ zonedTimeToLocalTime zt) s

--
-- Converts UTC to local time and then pretty prints it
--
prettyTime :: TimeZone -> UTCTime -> String
prettyTime tz t = formatTime defaultTimeLocale "%a, %d %b %y %H:%M:%S"
                    (utcToLocalTime tz t)

isoTime :: TimeZone -> UTCTime -> String
isoTime tz t = formatTime defaultTimeLocale "%F %X" (utcToLocalTime tz t)