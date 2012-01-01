{-# LANGUAGE OverloadedStrings #-}
module ClearCmd where

-- standard libraries

-- friends
import RecordSet

import Record (Record)
import qualified Record as R

--
-- Given a UTCTime (to determine what day it is) and a TimeZone returns option descriptions
-- for the start command
--
clearCmd ::  [String] -> IO ()
clearCmd _ = do
  clearCurrentRecord
  putStrLn "Clearing current task"
