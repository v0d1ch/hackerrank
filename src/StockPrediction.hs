{-# language ScopedTypeVariables #-}
module StockPrediction where

readMultipleLinesAsList :: Int -> IO [Int]
readMultipleLinesAsList 0 = return []
readMultipleLinesAsList n = do
  line <- read <$> getLine
  rest <- readMultipleLinesAsList (n - 1)
  return (line : rest)

main :: IO()
main = do
  arrLength :: Int <- read <$> getLine
  arr <- readMultipleLinesAsList arrLength
  print arr
