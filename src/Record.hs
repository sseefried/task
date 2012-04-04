{-# LANGUAGE OverloadedStrings #-}
module Record (
  Record(..), CurrentRecord(..),
  -- functions on records
  overlap, validateRecord, 
  -- abstract data type RecordSet
  RecordSet, 
  -- functions on RecordSet
  newRecordId,
  insert, empty, length, head, last, null, add, records, findBetween,
  setCurrent, current, isCurrent, clearCurrent, finishCurrent, lastN
) where

-- standard libraries
import Data.Time
import Data.Text (Text)
import qualified Data.Text as T

import Prelude hiding (length, head, last, null)
import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Data.Map (Map)
import qualified Data.Map as M

import Data.Foldable (toList)
import Text.Printf

import Data.Maybe
import System.Random (randomRIO)

data CurrentRecord =
  CurrentRecord { crecDescr     :: Text -- description
                , crecStart     :: UTCTime
                , crecKeyValues :: [ (Text, Text) ] } deriving Show

data Record =
  Record { recId        :: Text
         , recDescr     :: Text -- Description
         , recStart     :: UTCTime
         , recFinish    :: UTCTime
         , recKeyValues :: [ (Text, Text) ] } deriving Show

--
-- Abstract data type for record sets
--
-- For now this is implemented with Data.Sequence but this is probably not the
-- best considering the high cost of insertion.
--
data RecordSet =
  RecordSet { rsSeq     :: Seq Record
            , rsMap     :: Map Text Record
            , rsCurrent :: Maybe CurrentRecord }

instance Show RecordSet where
  show (RecordSet s _ c) = printf "RecordSet { rsSeq = %s, rsCurrent = %s }" (show s) (show c)

--
-- | Creates a new unique Id for a record in a record set.
-- 10 chars long. a-z
--
randomId :: IO Text
randomId = do
  s <- sequence $ map (const randomChar) [1..10]
  return $ T.pack s
  where
    randomChar = randomRIO ('a','z')

--
-- Generate a unique identifier that has not been used before.
--
newRecordId :: RecordSet -> IO Text
newRecordId rs = do
  s <- randomId
  case M.lookup s (rsMap rs) of
     Just _  -> newRecordId rs
     Nothing -> return s

--
-- | @insert r rs@ inserts record @r@ at the first point in sequence @rs@
-- where @r@ is after a record.
--
insert :: Record -> RecordSet -> Either String RecordSet
insert r rs
  | canInsert = Right $ rs  { rsSeq = (rsL S.|> r) S.>< rsR
                            , rsMap = M.insert (recId r) r (rsMap rs)}
  | otherwise =
    Left  $ printf "Record '%s' can't be inserted at any point in the record set" (show r)
  where
    canInsert = case S.viewl rsR of
                  S.EmptyL    -> True
                  rR S.:< _   -> rR `isAfter` r
    s = rsSeq rs
    (rsL,rsR) = S.breakl (r `isAfter`) s

--
-- | Finds all records that fit entirely between @start@ and @finish@ inclusive.
--   Does not return records that overlap with these times.
--
findBetween :: UTCTime -> UTCTime -> RecordSet -> [Record]
findBetween start finish rs = toList . S.takeWhileL p . S.dropWhileL (not . p) $ rsSeq rs
  where
    p r      = recStart r >= start && recFinish r <= finish

empty :: RecordSet
empty  = RecordSet S.empty M.empty Nothing

length :: RecordSet -> Int
length = S.length . rsSeq

head :: RecordSet -> Record
head rs = case S.viewl (rsSeq rs) of
  S.EmptyL -> error "RecordSet.head: empty RecordSet"
  r S.:< _ -> r

lastN :: Int -> RecordSet -> [Record]
lastN n rs = toList . snd $ S.splitAt (max 0 (S.length s - n)) s
  where s = rsSeq rs

last :: RecordSet -> Record
last rs = case S.viewr (rsSeq rs) of
  S.EmptyR -> error "RecordSet.last: empty RecordSet"
  _ S.:> r -> r

null :: RecordSet -> Bool
null = S.null . rsSeq

--
-- Adds record @r@ to @rs at the end of the sequence if
-- @r@ does not overlap with the last record.
-- Returns @Nothing@ otherwise.
--
add :: RecordSet -> Record -> Either String RecordSet
add rs r
  | null rs             = Right $ rs |> r
  | notUniqueIn r rs    = Left (printf "Record ID %s is already in record set" (show $ recId r))
  | overlap (last rs) r = Left (printf "Records overlap.\n  1. '%s'\n  2. '%s'"
                               (show . last $ rs) (show r))
  | not (r `isAfter` last rs) =
      Left (printf ("Record does not start at or after last record\n"++
                   "  1. %s\n  2. %s\n") (show r) (show . last $ rs))
  | otherwise           = Right $ rs |> r

records :: RecordSet -> [Record]
records = toList . rsSeq



--
-- Sets the current record. If one already existed it is lost.
--
setCurrent :: RecordSet -> CurrentRecord -> RecordSet
setCurrent rs cr = rs { rsCurrent = Just cr }

current :: RecordSet -> Maybe CurrentRecord
current = rsCurrent

isCurrent :: RecordSet -> Bool
isCurrent = isJust . rsCurrent

--
-- Clears the current record.
--
clearCurrent :: RecordSet -> RecordSet
clearCurrent rs = rs { rsCurrent = Nothing }


--
-- Adds record to the end of the sequence. Inserts the record into the rsMap
--
(|>) :: RecordSet -> Record -> RecordSet
rs |> r = rs { rsSeq = rsSeq rs S.|> r, rsMap = M.insert (recId r) r (rsMap rs) }

-------------

notUniqueIn :: Record -> RecordSet -> Bool
notUniqueIn r rs = isJust $ M.lookup (recId r) (rsMap rs)

--
-- There is overlap if the start or the finish of the second record
-- lies between the start and finish of the first record.
--
overlap :: Record -> Record -> Bool
overlap r r' = inBoundary (recStart r') || inBoundary (recFinish r')
  where
    inBoundary t = recStart r < t && t < recFinish r

-- | Checks whether @r@ occurs after @r'@
isAfter :: Record -> Record -> Bool
isAfter r r' = recStart r >= recFinish r'

--
-- Finishes the current task in the record set, if and only if,
-- there is a current record. Otherwise returns the original
-- RecordSet.
--
finishCurrent :: RecordSet -> UTCTime -> IO RecordSet
finishCurrent rs t = do
  i <- newRecordId rs
  let maybeCR = current rs
  case maybeCR of
    Just cr -> do
      let rs'     = clearCurrent rs
          eitherRS = rs' `add` (Record { recId        = i
                                       , recStart     = crecStart cr
                                       , recFinish    = t
                                       , recDescr     = crecDescr cr
                                       , recKeyValues = crecKeyValues cr })
      case eitherRS of
        Left _     -> return rs -- return the original
        Right rs'' -> return rs''
    Nothing -> return rs
--
-- Either returns the record (because it is valid) or returns an error message.
--
-- Things that can go wrong.
-- 1. finish is before start time.
-- 2. key in key/values is one of "id", "start", "finish"
-- FIXME: Not finished
validateRecord :: Record -> Either String Record
validateRecord r
  | recStart r >= recFinish r =
    Left (printf "Start time '%s' is equal to or after finish time '%s'"
         (show $ recStart r) (show $ recFinish r))
  | containsReservedKey (recKeyValues r) =
      Left (printf "Key/Values '%s' contains one of the reserved keys: %s"
           (show . recKeyValues $ r) (show $ reservedKeys))
  | otherwise = Right r
  where
    containsReservedKey kvs = any ((`elem` reservedKeys) . fst) kvs

reservedKeys = [ "id", "start", "finish"]