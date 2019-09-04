module RansomNote where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- Complete the checkMagazine function below.
checkMagazine :: [String] -> [String] -> IO ()
checkMagazine magazine note =
  let magMap = mapFromList magazine
      noteMap = mapFromList note
      checkNumbers =
        (\n ->
          let magNo = M.lookup n magMap
              noteNo = M.lookup n noteMap
          in magNo >= noteNo
        ) <$> note
  in
    if False `elem` checkNumbers
      then putStrLn "No"
      else putStrLn "Yes"

mapFromList :: [String] -> Map String Int
mapFromList l =
  foldl (\m str ->
      case M.lookup str m of
        Nothing -> M.insert str 1 m
        Just _ -> M.update (\no -> Just (no + 1)) str m
  ) M.empty l

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
  line <- getLine
  rest <- readMultipleLinesAsStringArray(n - 1)
  return (line : rest)

main :: IO()
main = do
  magazineTemp <- getLine
  let magazine = words magazineTemp
  noteTemp <- getLine
  let note = words noteTemp
  checkMagazine magazine note
