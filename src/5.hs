module Main where

import Data.List
import Data.Sequence (Seq, fromList, index, update)

main :: IO ()
main = do
  xs <- (map (read::String->Int) . lines) <$> readFile  "inputs/input5.txt"
  print $ steps 0 0 (+1) (fromList xs)
  print $ steps 0 0 (\x -> if x >= 3 then x-1 else x+1) (fromList xs)


steps:: Int -> Int -> (Int->Int) -> Seq Int -> Int
steps n i f s
  | i < 0 || i >= length s = n
  | otherwise = steps (n+1) (i+v) f (update i (f v) s)
      where v = index s i
 
