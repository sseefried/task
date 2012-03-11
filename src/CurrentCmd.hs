{-# LANGUAGE OverloadedStrings #-}
module CurrentCmd where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf

-- friends
import Time
import Record
import RecordSet

-- This command doesn't actually use any command line arguments
-- but still respects the spec.
currentCmd :: ZonedTime -> [String] -> IO ()
currentCmd zt _ = do
  mbRecord <- readCurrentRecord
  case mbRecord of
    Just r  -> printf "Description:  %s\nStarted:      %s\n"
                  (T.unpack $ crecDescr  r)
                  (prettyTime (zonedTimeZone zt) (crecStart r))
    Nothing -> printf "There is no current record.\n"
