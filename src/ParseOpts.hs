module ParseOpts where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Char8
import Data.Aeson.Parser (jstring)
import Data.Aeson
import Control.Applicative
import Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.Zepto as Z
import Data.ByteString.Unsafe as B
import Data.Time
import Text.Printf
import Data.Either

-- friends
import Time


--
-- Parses a key/value flag
--
parseKeyValueFlag :: String -> Either String (Text,Text)
parseKeyValueFlag s = case parse parser . BS.pack  $ s of
  Done _ r   -> Right r
  Partial _  -> Left "Unexpected end of input on key/value pair"
  Fail s _ _ -> Left (printf "Error parsing key/value near: '%s'."
                     (BS.unpack . BS.takeWhile (not . (=='\n')) $ s))

  where
    parser :: Parser (Text,Text)
    parser = do
      key   <- jstring <* char ':'
      value <- jstring
      return (key,value)

--
-- Parses the time flag ensuring the time is not in the future. The first argument must be
-- the current time.
--
parseTimeFlag :: ZonedTime -> String -> Either String UTCTime
parseTimeFlag zt s = case parseTaskTime zt s of
  Nothing      -> Left (printf "Could not parse time string `%s'." $ s)
  Just utcTime -> case utcTime > zonedTimeToUTC zt of
    True  -> Left $ printf "Invalid time flag value '%s'. It's in the future." s
    False -> Right utcTime
