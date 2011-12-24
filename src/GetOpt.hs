
--
-- A small wrapper for GetOpts
--
module GetOpt (
  module System.Console.GetOpt,

  getOptEither) where

-- standard libraries
import System.Console.GetOpt
import Data.Either

getOptEither :: ArgOrder (Either String a)   -- non-option handling
             -> [OptDescr (Either String a)] -- option descriptors
             -> [String]                     -- the command-line arguments
             -> ([a],[String],[String])      -- (options,non-options,error messages)
getOptEither argOrder optDescrs args =
  (opts, nonOpts, moreErrors ++ errors)
  where
    (moreErrors, opts) = partitionEithers eitherOpts
    (eitherOpts, nonOpts, errors) = getOpt argOrder optDescrs args

