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
prettyTime = prettyFmtTime "%a, %d %b %y %H:%M:%S"

prettyFmtTime :: String -> TimeZone -> UTCTime -> String
prettyFmtTime fmt tz t = formatTime defaultTimeLocale fmt (utcToLocalTime tz t)


isoTime :: TimeZone -> UTCTime -> String
isoTime tz t = formatTime defaultTimeLocale "%F %X" (utcToLocalTime tz t)

sameDayAs :: TimeZone -> UTCTime -> UTCTime -> Bool
sameDayAs tz t t' =  f t == f t'
  where f = prettyFmtTime "%d %b %Y" tz

daysHoursMinutesSeconds :: Integer -> String
daysHoursMinutesSeconds s =
  let (days,s') = s `divMod` 86400
      (hours,s'') = s' `divMod` 3600
      (mins, secs) = s'' `divMod` 60
  in if       days  > 0 then printf "%dd%02dh%02dm%02ds" days hours mins secs
     else (if hours > 0 then printf      "%dh%02dm%02ds" hours mins secs
     else (if mins > 0 then  printf           "%dm%02ds" mins secs
     else                    printf                "%ds" secs))



