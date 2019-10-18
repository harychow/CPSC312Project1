module RandNumGen where

import System.Random

randRange:: Int -> Int -> IO Int
randRange lo hi = do
  num1 <- randomRIO(lo, hi)
  return num1
