module Record (
  Record(..),
  -- functions on records
  overlap, validateRecord,
  -- abstract data type RecordSet
  RecordSet, 
  -- functions on RecordSet
  insert, empty, length, head, last, null, add
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

data Record = Record { recId        :: Text
                     , recStart     :: UTCTime
                     , recFinish    :: UTCTime
                     , recKeyValues :: [ (Text, Text) ] } deriving Show

--
-- Abstract data type for record sets
--
-- For now this is implemented with Data.Sequence but this is probably not the
-- best considering the high cost of insertion.
--
data RecordSet = RecordSet { rsSeq :: Seq Record, rsMap :: Map Text Record }

instance Show RecordSet where
  show (RecordSet s _) = show s

--
-- | 'insert p v s' inserts value 'v' at the first point in sequence 's'
--   where predicate 'p' is 'True'.
--
insert :: (Record -> Bool) -> Record -> RecordSet -> RecordSet
insert p r rs = rs  { rsSeq = (rsL S.|> r) S.>< rsR
                    , rsMap = M.insert (recId r) r (rsMap rs)}
  where
    s = rsSeq rs
    (rsL,rsR) = S.breakl p s

--
-- | Finds all records that fit entirely between @start@ and @finish@ inclusive.
--   Does not return records that overlap with these times.
--
findBetween :: UTCTime -> UTCTime -> RecordSet -> [Record]
findBetween start finish rs = toList . S.takeWhileL p . S.dropWhileL (not . p) $ rsSeq rs
  where
    p r      = recStart r >= start && recFinish r <= finish

empty :: RecordSet
empty  = RecordSet S.empty M.empty

length :: RecordSet -> Int
length = S.length . rsSeq

head :: RecordSet -> Record
head rs = case S.viewl (rsSeq rs) of
  S.EmptyL -> error "RecordSet.head: empty RecordSet"
  r S.:< _ -> r

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
  | null rs             = Right $ rs { rsSeq = S.singleton r, rsMap = rsMap' }
  | notUniqueIn r rs    = Left (printf "Record ID %s is already in record set" (show $ recId r))
  | overlap (last rs) r = Left (printf "Records '%s' and '%s' overlap"
                               (show . last $ rs) (show r))
  | otherwise           = Right $ rs { rsSeq = rsSeq rs S.|> r, rsMap = rsMap' }
  where rsMap' = M.insert (recId r) r (rsMap rs)


notUniqueIn :: Record -> RecordSet -> Bool
notUniqueIn r rs = isJust $ M.lookup (recId r) (rsMap rs)

--
-- There is overlap if the start or the finish of the second record
-- lies between the start and finish of the first record.
--
overlap :: Record -> Record -> Bool
overlap r r' = inBoundary (recStart r') || inBoundary (recFinish r')
  where
    inBoundary t = recStart r <= t && t <= recFinish r

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