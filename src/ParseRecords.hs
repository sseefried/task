{-# LANGUAGE OverloadedStrings #-}
--
-- Author: Sean Seefried
-- Date:   Wed 21 Dec 2011
--

--
-- | Defines data type and function for the 'Record' data type.
--
module ParseRecords (
  readRecordSet, writeRecordSet
) where

-- standard libraries
import Data.Aeson
import Data.Aeson.Types                       (parseEither)
import Control.Monad                          (mzero, when, liftM)
import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Attoparsec as AP
import Data.Either                            (either)
import Data.Maybe                             (isNothing)
import Data.List                              (intersperse)
import Text.Printf                            (printf)
import Prelude hiding (null)
import qualified Prelude as P
import System.Exit                            (exitWith, ExitCode(..))
import System.Environment                     (getEnv, getEnvironment)
import System.Directory                       (doesDirectoryExist)
import System.Posix.Files                     (fileExist)
import System.FilePath                        ((</>))

-- friends
import Config
import Record
import qualified Record as R

instance FromJSON Record where
  parseJSON (Object v) =
        Record <$> (v .: "id")
               <*> (v .: "start"      >>= parseJSON)
               <*> (v .: "finish"     >>= parseJSON)
               <*> (v .: "key_values" >>= parseJSON)
  parseJSON _ = mzero

instance ToJSON Record where
  toJSON (Record id start finish keyValues) =
    object ["id" .= id, "start" .= start, "finish" .= finish, "key_values" .= keyValues]

instance FromJSON CurrentRecord where
  parseJSON (Object v) =
    CurrentRecord <$> (v .: "start"      >>= parseJSON)
                  <*> (v .: "key_values" >>= parseJSON)
  parseJSON _ = mzero

instance ToJSON CurrentRecord where
  toJSON (CurrentRecord start keyValues) =
    object [ "start" .= start, "key_values" .= keyValues ]

--
-- Parse a record and return either the record or an error and the remainder of the input (if any)
--
parseRecord :: BS.ByteString -> (Either String Record, BS.ByteString)
parseRecord bs =
  case AP.parse json bs of
    AP.Done rest r -> case parseEither parseJSON r of
      Left err -> (Left err,            rest)
      Right r  -> (validateRecord r,  rest)
    AP.Partial _   -> (Left "Unexpected end of input", "")
    AP.Fail s _ _  -> (Left (printf "Error near: '%s'"
                       (BS.unpack . BS.takeWhile (not . (=='\n')) $ s)), "")

--
-- FIXME: Records can actually be spread over multiple lines but we don't take that into account
-- with our line numbers.
--
parseRecords :: BS.ByteString -> Either String RecordSet
parseRecords str =
  either
    (Left . printf "ERROR: Couldn't parse records file '%s'\n%s" recordsFile . unlines)
    Right
    (go 1 str [] R.empty)
  where
    go :: Int -> BS.ByteString -> [String] -> RecordSet -> Either [String] RecordSet
    go lineNumber s errs rs
      | BS.all whiteSpace s = if P.null errs then Right rs else Left (reverse errs)
      | otherwise =
        case result of
          Left err -> go (lineNumber+1) s' (addLineNumber err:errs) rs
          Right r  -> case rs `R.add` r of
            Left  err -> go (lineNumber+1) s' (addLineNumber err:errs) rs
            Right rs' -> go (lineNumber+1) s' errs rs'
        where
          (result, s') = parseRecord s
          whiteSpace c = c `elem` "\n\r\t\f "
          addLineNumber err = printf "%d:%s" lineNumber err

parseRecordFile :: FilePath -> IO (Either String RecordSet)
parseRecordFile path = BS.readFile path >>= (return . parseRecords)

parseCurrentRecord :: BS.ByteString -> Maybe CurrentRecord
parseCurrentRecord bs = case AP.parse json bs of
  AP.Done rest r -> either (const Nothing) Just (parseEither parseJSON r)
  _              -> Nothing

parseCurrentRecordFile :: FilePath -> IO (Maybe CurrentRecord)
parseCurrentRecordFile path = BS.readFile path >>= (return . parseCurrentRecord)

--
-- Returns (<recordFilePath>, <currentRecordFilePath>)
--
checkAndGetFilePaths :: IO (String, String)
checkAndGetFilePaths = do
  env     <- getEnvironment
  exitWithErrorIf (isNothing $ lookup taskDirEnvVar env)
    (printf "%s environment variable does not exist" taskDirEnvVar)
  taskDir <- getEnv taskDirEnvVar
  exitWithErrorIf (P.null taskDir)
    (printf "%s environment variable is empty" taskDirEnvVar)
  exitWithErrorIfM (liftM not . doesDirectoryExist $ taskDir)
    (printf "%s directory '%s' does not exist" taskDirEnvVar taskDir)
  let recordsFilePath       = taskDir </> recordsFile
      currentRecordFilePath = taskDir </> currentRecordFile
  return (recordsFilePath, currentRecordFilePath)

--
-- | Reads the 'RecordSet' from the relevant files.
--
-- Exits with failure on a number of erroneous conditions.
--
readRecordSet :: IO RecordSet
readRecordSet = do
  (recordsFilePath, currentRecordFilePath) <- checkAndGetFilePaths
  createFileIfMissing recordsFilePath
  createFileIfMissing currentRecordFilePath
  eitherRS  <- parseRecordFile recordsFilePath
  rs        <- getOrExitWithError eitherRS
  mbCurrent <- parseCurrentRecordFile currentRecordFilePath
  return $ maybe rs (setCurrent rs) mbCurrent -- possibly add the current record
  where
    getOrExitWithError :: Either String a -> IO a
    getOrExitWithError = either (\s -> putStr s >> exitWith (ExitFailure 1)) return

writeRecordSet :: RecordSet -> IO ()
writeRecordSet rs = do
  (recordsFilePath, currentRecordFilePath) <- checkAndGetFilePaths
  -- TODO: Could be a bit slow. Make faster
  BSL.writeFile recordsFilePath (BSL.concat . intersperse "\n" .
                                 map (encode . toJSON) $ records rs)
  case current rs of
    Just cr -> BSL.writeFile currentRecordFilePath (encode . toJSON $ cr)
    Nothing -> return ()

-- Some helpers

exitWithErrorIf :: Bool -> String -> IO ()
exitWithErrorIf condition reason = when condition $ do
  putStrLn reason
  exitWith (ExitFailure 1)

exitWithErrorIfM :: IO Bool -> String -> IO ()
exitWithErrorIfM ioCond reason = ioCond >>= \b -> exitWithErrorIf b reason

createFileIfMissing path = do
  exists <- fileExist path
  when (not exists) $ do
    printf "'%s' does not exist. Creating.\n" path
    writeFile path ""
