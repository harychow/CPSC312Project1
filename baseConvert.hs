module BaseConvert where

digitList:: [Char]
digitList = ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']

-- always feed convert the empty list as acc
-- remember to reverse the output of convert whenever it's used
convert :: Int -> Int -> [Char] -> [Char]
convert n b acc =
    if (div n b == 0)
        then ((digitList!!(mod n b)):acc)
        else (digitList!!(mod n b)):(convert (div n b) b acc)

-- this function is copied from
-- https://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell/26961027
bin2dec :: String -> Int
bin2dec =
  foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
        where c2i c = if c == '0' then 0 else 1
