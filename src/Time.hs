{-# LANGUAGE PatternGuards #-}
module Time (
  module Data.Time,
  module Time) where


-- standard libraries
import Data.Time
import System.Locale (defaultTimeLocale)
import Text.Printf

data TimeWithZone = TimeWithZone UTCTime TimeZone


--
-- Parse the task time, trying a variety of different formats.
--
parseTaskTime :: TimeWithZone -> String -> Maybe LocalTime
parseTaskTime (TimeWithZone t tz) s
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
    p fmt = parseTime defaultTimeLocale fmt
    full  = printf "%s %s" (formatTime defaultTimeLocale "%Y-%m-%d" t) s
