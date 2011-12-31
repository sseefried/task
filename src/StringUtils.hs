module StringUtils where

import Data.List


-- Removes all leading and trailing whitespace from a String
strip :: String -> String
strip = reverse . dropWhite . reverse . dropWhite
  where
    dropWhite = dropWhile (`elem` whiteChars)
    whiteChars = " \t\r\n"

spaces :: Int -> String
spaces n = take n (repeat ' ')

indent :: Functor f => Int -> f String -> f String
indent n = fmap (spaces n ++)

padTo :: Int -> String -> String
padTo n s = if len < n then s ++ spaces (n - len) else s
 where
   len = length s
--
-- Takes a list of pairs of strings and lays them out
-- in a 2-column format. The argument 'n' is the width of
-- the first column
--
sepColumns :: Int -> [(String, String)] -> [String]
sepColumns n = map join
  where join (s, s') = padTo n s ++ s'

twoColumns :: Int -> [(String, String)] -> [String]
twoColumns n xs = sepColumns (maxLen + n) xs
  where maxLen = maximum . map (length . fst) $ xs