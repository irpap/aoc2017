module Main where

import Data.List

main :: IO ()
main = do
  xs <- (map (read::String->Int) . words) <$> readFile  "inputs/input6.txt"
  print $ part1 xs
  print $ part2 xs

next::[Int] -> [Int]
next xs =
  let maxIndex = head (elemIndices (maximum xs) xs)
      (q, r) = divMod (xs !! maxIndex) (length xs)
      i1 = mod (maxIndex+1) (length xs)
      i2 = mod (maxIndex+r+1) (length xs)
      newValue x i
        | i1 <= i2 && i >= i1 && i < i2 = y + q + 1
        | i1 > i2 && (i >= i1 || i < i2) = y + q + 1
        | otherwise = y + q
          where y = if i == maxIndex then 0 else x in
  zipWith newValue xs [0..]

run:: [[Int]] -> [[Int]]
run y@(x:xs) = if next x `elem` y then next x:y else run (next x:y)

part1 xs= length (run [xs]) - 1

part2 xs = (inds!!1) - (inds!!0)
  where inds = elemIndices (head s) s
        s = run [xs]
