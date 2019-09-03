{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module RansomNote where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the checkMagazine function below.
checkMagazine magazine note = undefined

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
  line <- getLine
  rest <- readMultipleLinesAsStringArray(n - 1)
  return (line : rest)

main :: IO()
main = do
  mnTemp <- getLine
  let mn = words mnTemp

  let m = read (mn !! 0) :: Int

  let n = read (mn !! 1) :: Int

  magazineTemp <- getLine

  let magazine = words magazineTemp

  noteTemp <- getLine

  let note = words noteTemp

  checkMagazine magazine note
