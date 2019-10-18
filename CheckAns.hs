module CheckAns where

import Data.List
import BaseConvert

checkans_binStr_decInt:: (Show a) => a -> Int -> Bool
checkans_binStr_decInt intB intD =
  n1 == n2
  where n1 = bin2dec (show intB)
        n2 = intD
