-- https://www.hackerrank.com/challenges/fp-filter-positions-in-a-list/problem?h_r=next-challenge&h_v=zen
module OddPositions where

import Safe

f :: [Int] -> [Int]
f lst = do
  let evens = [1,3..]
  (\index ->
      if index <= length lst
        then lst !! index
        else 0
   ) <$> (take ((length lst) `div` 2) evens)

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main :: IO ()
main = do
    inputdata <- getContents
    mapM_ (putStrLn. show). f. map read. lines $ inputdata
