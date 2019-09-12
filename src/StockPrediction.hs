{-# LANGUAGE ScopedTypeVariables #-}

module StockPrediction where

readMultipleLinesAsList :: Int -> IO [Int]
readMultipleLinesAsList 0 = return []
readMultipleLinesAsList n = do
  line <- read <$> getLine
  rest <- readMultipleLinesAsList (n - 1)
  return (line : rest)

readALine :: IO Int
readALine = read <$> getLine

main :: IO ()
main = do
  arrLength <- readALine
  arr <- readMultipleLinesAsList arrLength
  queryNumber <- readALine
  let noOfQueries = replicate queryNumber 1
  indexAndMargin <-
    mapM (const (readMultipleLinesAsList 2)) noOfQueries
  putStrLn "============"
  print arrLength
  print arr
  print queryNumber
  print indexAndMargin
