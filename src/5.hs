module Main where

import Data.List
import Data.Sequence (Seq, fromList, index, update)

main :: IO ()
main = do
  xs <- (map (read::String->Int) . lines) <$> readFile  "inputs/input5.txt"
  print $ steps 0 0 (fromList xs)

steps:: Int -> Int -> Seq Int -> Int
steps n i s
  | i < 0 || i >= length s = n
  | otherwise = steps (n+1) (i+v) (update i (v+1) s)
      where v = index s i
