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
import Data.Maybe
import Text.Printf
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import Control.Monad
import System.Exit
import Data.Map (Map)
import qualified Data.Map as Map


-- friends
import StringUtils
import Time
import StartCmd 
import ModifyCmd

main :: IO ()
main = do
  twz <- getTimeWithZone
  name <- getProgName
  args <- getArgs
  let cmdMap = commandMap name twz
      dispatch (cmd:argsForCmd) =
        case Map.lookup cmd cmdMap  of
          Just cmd -> cmdHandler cmd argsForCmd
          Nothing -> printf "%s: '%s' is not a command. See '%s --help'.\n" name cmd name
  when (wantsHelp args) $ do
    putStr $ topLevelHelp name twz
    exitWith ExitSuccess
  let (cmd:restArgs) = args
  when (cmd == "help" && length restArgs > 0) $ do
    putStrLn $ helpFor (head restArgs) cmdMap
    exitWith ExitSuccess
  -- now dispatch
  dispatch args

helpFor :: String -> Map String Command -> String
helpFor cmd cmdMap = case Map.lookup cmd cmdMap of
  Just cmd -> cmdHelp cmd
  Nothing  -> printf "Can't get help for unknown command '%s'" cmd

--------------

data Command = Cmd { cmdId :: String
                   , cmdDesc :: String
                   , cmdHelp :: String
                   , cmdHandler :: [String] -> IO () }

commands :: String -> TimeWithZone -> [Command]
commands name twz =
  [ Cmd "start"
        "Start a new task"
        (usageInfo (printf "Usage: %s start [<flags>...]\n\nFlags:" name)
                   (startCmdOpts twz))
        (startCmd twz)
  , Cmd "finish"
        "Finish current task"
        (error "not defined")
        undefined
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
        (error "not defined")
        undefined
  ]

commandMap :: String -> TimeWithZone -> Map String Command
commandMap name twz = foldl (\m cmd -> Map.insert (cmdId cmd) cmd m) Map.empty (commands name twz)

-----------

topLevelHelp :: String -> TimeWithZone -> String
topLevelHelp name twz = unlines $ [
    printf "Usage: %s <command> <flags...>" name
  , ""
  , "Commands:"
  ] ++ (indent 2 . twoColumns 4 $ map f $ commands name twz) ++
  [ ""
  , printf "See '%s help <command>' for more information on a specific command." name]

 where f cmd = (cmdId cmd, cmdDesc cmd)

getTimeWithZone :: IO TimeWithZone
getTimeWithZone = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ TimeWithZone t tz

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
  ct@(TimeWithZone _ tz) <- getTimeWithZone
  let mtime = parseTaskTime ct "00:23"
  case mtime of
    Just time -> printf "Local: %s\nUTC: %s\n" (show time) (show $ localTimeToUTC tz time)
    Nothing    -> return ()
