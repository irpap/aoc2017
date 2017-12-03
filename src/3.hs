module Main where

import           Data.Char
import           Data.List
import           Data.List.Split

main = do
  let n = read "347991"::Int
  print $ minsteps $ directions n
  print $ firstgreater n

-- Number of steps in each direction [right, up, left, down, ..] to go from 1 to n
directions:: Int -> [Int]
directions 1 = []
directions n =
  let d = concatMap (replicate 2 ) [1..]
      acc = scanl (+) 1 d
      len = length $ takeWhile (n > ) acc
    in take (len - 1 ) d ++ [n - (acc !! (len - 1))]

d :: [Int] -> Int -> Int
d ds n | n < length t = sum $ t !! n
       | otherwise = 0
  where t = (transpose . chunksOf 4) ds

minsteps::[Int] -> Int
minsteps ds = abs (right - left) + abs (up - down)
  where right = d ds 0
        up = d ds 1
        left = d ds 2
        down = d ds 3

stepsbetween::Int -> Int  -> Int
stepsbetween a b = max (abs (right - left)) (abs (up - down))
                  where right = d (directions a) 0 -  d (directions b) 0
                        up = d (directions a) 1 - d (directions b) 1
                        left = d (directions a) 2 - d (directions b) 2
                        down = d (directions a) 3 - d (directions b) 3

neighbours::Int -> [Int]
neighbours n = filter (\x -> stepsbetween x n == 1) [1..n]

accsum = (map nsum [0..] !! )
  where nsum 1 = 1
        nsum n = sum $ map accsum (neighbours n)

firstgreater n = head $ dropWhile (<= n) (map accsum [1..])
