module Errors where

import Control.Monad (when)
import System.Exit (exitWith, ExitCode(..))


exitWithErrorIf :: Bool -> String -> IO ()
exitWithErrorIf condition reason = when condition $ do
  putStr reason
  exitWith (ExitFailure 1)

exitWithErrorIfM :: IO Bool -> String -> IO ()
exitWithErrorIfM ioCond reason = ioCond >>= \b -> exitWithErrorIf b reason
