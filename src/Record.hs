module Record where

-- standard libraries
import Data.Time
import Data.Text (Text)

import Data.Sequence (Seq)
import qualified Data.Sequence as S

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
data RecordSet = RecordSet (Seq Record)
