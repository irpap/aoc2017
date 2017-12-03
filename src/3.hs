module Main where

import           Data.Char

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

every [] _ = []
every (x:xs) n = x : f xs n
  where f xs n = case drop (n-1) xs of
                  (y:ys) -> y : f ys n
                  [] -> []

minsteps::[Int] -> Int
minsteps d = abs (right - left) + abs (up - down)
  where right = sum $ every d 4
        up = sum $ every (drop 1 d) 4
        left = sum $ every (drop 2 d) 4
        down = sum $ every (drop 3 d) 4

stepsbetween::Int -> Int  -> Int
stepsbetween a b = max (abs (right - left)) (abs (up - down))
                  where right = sum (every (directions a) 4) - sum (every (directions b) 4)
                        up = sum (every (drop 1 (directions a)) 4) - sum (every (drop 1 (directions b)) 4)
                        left = sum (every (drop 2 (directions a)) 4) - sum (every (drop 2 (directions b)) 4)
                        down = sum (every (drop 3 (directions a)) 4) - sum (every (drop 3 (directions b)) 4)

neighbours::Int -> [Int]
neighbours n = filter (\x -> stepsbetween x n == 1) [1..n]

accsum = (map nsum [0..] !! )
  where nsum 1 = 1
        nsum n = sum $ map accsum (neighbours n)

firstgreater n = head $ dropWhile (<= n) (map accsum [1..])
