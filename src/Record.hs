module Record (
  Record(..),
  -- functions on records
  overlap,
  -- abstract data type RecordSet
  RecordSet, 
  -- functions on RecordSet
  insert, empty, length, head, last, null, add
) where

-- standard libraries
import Data.Time
import Data.Text (Text)

import Prelude hiding (length, head, last, null)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Foldable

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
data RecordSet = RecordSet { unR :: Seq Record }

instance Show RecordSet where
  show (RecordSet s) = show s

--
-- | 'insert p v s' inserts value 'v' at the first point in sequence 's'
--   where predicate 'p' is 'True'.
--
insert :: (Record -> Bool) -> Record -> RecordSet -> RecordSet
insert p v (RecordSet s) = RecordSet $ (xs S.|> v) S.>< ys
  where
    (xs,ys) = S.breakl p s

--
-- | Finds all records that fit entirely between @start@ and @finish@ inclusive.
--   Does not return records that overlap with these times.
--
findBetween :: UTCTime -> UTCTime -> RecordSet -> [Record]
findBetween start finish (RecordSet s) = toList . S.takeWhileL p . S.dropWhileL (not . p) $ s
  where
    p r      = recStart r >= start && recFinish r <= finish

empty :: RecordSet
empty  = RecordSet S.empty

length :: RecordSet -> Int
length = S.length . unR

head :: RecordSet -> Record
head (RecordSet s) = case S.viewl s of
  S.EmptyL -> error "RecordSet.head: empty RecordSet"
  r S.:< _ -> r

last :: RecordSet -> Record
last (RecordSet s) = case S.viewr s of
  S.EmptyR -> error "RecordSet.last: empty RecordSet"
  _ S.:> r -> r

null :: RecordSet -> Bool
null = S.null . unR

--
-- Adds record @r@ to @rs at the end of the sequence if
-- @r@ does not overlap with the last record.
-- Returns @Nothing@ otherwise.
--
add :: RecordSet -> Record -> Maybe RecordSet
add rs r
  | null rs             = Just . RecordSet $ S.singleton r
  | overlap (last rs) r = Nothing
  | otherwise           = Just . RecordSet $ (unR rs) S.|> r


--
-- There is overlap if the start or the finish of the second record
-- lies between the start and finish of the first record.
--
overlap :: Record -> Record -> Bool
overlap r r' = inBoundary (recStart r') || inBoundary (recFinish r')
  where
    inBoundary t = recStart r <= t && t <= recFinish r