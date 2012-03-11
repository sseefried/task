{-# LANGUAGE OverloadedStrings, PatternGuards #-}
--
-- Author: Sean Seefried
-- Date:   Wed 21 Dec 2011
--
module Main where

-- library imports
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf
import System.Environment (getArgs, getProgName)
import Control.Monad
import System.Exit
import Data.Map (Map)
import qualified Data.Map as Map

-- friends
import StringUtils
import Time

-- The commands
import StartCmd 
import ModifyCmd
import ClearCmd
import FinishCmd
import ExportCmd
import CurrentCmd

import GetOpt

main :: IO ()
main = do
  zt <- getZonedTime
  name <- getProgName
  args <- getArgs
  let cmdMap = commandMap name zt
      dispatch (cmd:argsForCmd) =
        case Map.lookup cmd cmdMap  of
          Just cmd -> cmdHandler cmd argsForCmd
          Nothing -> printf "%s: '%s' is not a command. See '%s --help'.\n" name cmd name
  when (wantsHelp args) $ do
    putStr $ topLevelHelp name zt
    exitWith ExitSuccess
  let (cmd:restArgs) = args
  when (cmd == "help" && length restArgs > 0) $ do
    putStrLn $ helpFor (head restArgs) cmdMap
    exitWith ExitSuccess
  -- now dispatch
  dispatch args

helpFor :: String -> Map String Command -> String
helpFor cmd cmdMap = case Map.lookup cmd cmdMap of
  Just cmd -> printf "%s\n\n%s" (cmdDesc cmd) (cmdHelp cmd)
  Nothing  -> printf "Can't get help for unknown command '%s'" cmd

--------------

data Command = Cmd { cmdId :: String
                   , cmdDesc :: String
                   , cmdHelp :: String
                   , cmdHandler :: [String] -> IO () }

--
-- Note to maintainer:
--
-- The 'cmdHelp' string should not end in a newline.
--
commands :: String -> ZonedTime -> [Command]
commands name zt =
  [ Cmd "start"
        "Start a new task"
        (usageInfo (printf "Usage: %s start [<flags>...]\n\nFlags:" name)
                   (startCmdOpts zt))
        (startCmd zt)
  , Cmd "current"
        "Describe the current task"
        (usageInfo (printf "Usage: %s current" name) [])
        (currentCmd zt)
  , Cmd "clear"
        "Clear current task"
        (printf "Usage: %s clear" name)
        clearCmd
  , Cmd "finish"
        "Finish current task"
        (usageInfo (printf "Usage: %s finish [<flags>...]\n\nFlags:" name)
                   (finishCmdOpts zt))
        (finishCmd zt)
  , Cmd "modify"
        "Modify a task entry"
        (error "not defined")
        undefined
  , Cmd "delete"
        "Delete a task entry"
        (error "not defined")
        undefined
  , Cmd "query"
        "Query task entries"
        (error "not defined")
        undefined
  , Cmd "export"
        "Export task data in a variety of formats"
        (usageInfo (printf "Usage: %s export [<flags>...]\n\nFlags:" name)
                   (exportCmdOpts zt))
        (exportCmd zt)

  ]

commandMap :: String -> ZonedTime -> Map String Command
commandMap name zt = foldl (\m cmd -> Map.insert (cmdId cmd) cmd m) Map.empty (commands name zt)

-----------

topLevelHelp :: String -> ZonedTime -> String
topLevelHelp name zt = unlines $ [
    printf "Usage: %s <command> <flags...>" name
  , ""
  , "Commands:"
  ] ++ (indent 2 . twoColumns 4 $ map f $ commands name zt) ++
  [ ""
  , printf "See '%s help <command>' for more information on a specific command." name]

 where f cmd = (cmdId cmd, cmdDesc cmd)

wantsHelp :: [String] -> Bool
wantsHelp args = containsHelp args || length args == 0 || (head args == "help" && length args == 1)

containsHelp :: [String] -> Bool
containsHelp = any pred
  where
    pred s = any (eq s) ["-h", "--help", "-?"]
    eq s s' = strip s == s'
------------------------------

test :: IO ()
test = do
  zt <- getZonedTime
  let mtime = parseTaskTime zt "00:23"
  case mtime of
    Just time -> printf "Local: %s\nUTC: %s\n"
                   (show $ utcToZonedTime (zonedTimeZone zt) time) (show time)
    Nothing    -> return ()
