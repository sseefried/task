{-# LANGUAGE PatternGuards #-}
module Time (
  module Data.Time,
  module Time) where


-- standard libraries
import Data.Time
import System.Locale (defaultTimeLocale)
import Text.Printf

--
-- Parse the task time, trying a variety of different formats.
--
parseTaskTime :: ZonedTime -> String -> Maybe ZonedTime
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
    tz = zonedTimeZone zt
    p fmt s = fmap (flip ZonedTime tz) (parseTime defaultTimeLocale fmt s)
    full    = printf "%s %s" (formatTime defaultTimeLocale "%Y-%m-%d" $ zonedTimeToLocalTime zt) s

--
-- Converts UTC to local time and then pretty prints it
--
prettyTime :: UTCTime -> TimeZone -> String
prettyTime t tz = formatTime defaultTimeLocale "%a, %d %b %y %H:%M:%S"
                    (utcToLocalTime tz t)