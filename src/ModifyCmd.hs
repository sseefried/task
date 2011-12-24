module ModifyCmd where

import Data.Text (Text)
import qualified Data.Text as T

-- friends
import Time

--
-- | Flags for the "modify" command
--
data ModifyCmdFlag =
    ModifyCmdId     Text
  | ModifyCmdStart  LocalTime
  | ModifyCmdFinish LocalTime
