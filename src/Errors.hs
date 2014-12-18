module Errors where

import Control.Monad (when)
import System.Exit (exitWith, ExitCode(..))


exitWithError :: String -> IO a
exitWithError s = putStrLn s >> exitWith (ExitFailure 1)

--
-- Adds a new line on the end of the error message.
--
exitWithErrorIf :: Bool -> String -> IO ()
exitWithErrorIf condition reason = when condition $ do
  putStrLn reason
  exitWith (ExitFailure 1)

-- A version that doesn't add a trailing new line
exitWithErrorIf' :: Bool -> String -> IO ()
exitWithErrorIf' condition reason = when condition $ do
  putStr reason
  exitWith (ExitFailure 1)


exitWithErrorIfM :: IO Bool -> String -> IO ()
exitWithErrorIfM ioCond reason = ioCond >>= \b -> exitWithErrorIf b reason
