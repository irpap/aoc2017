module Main where

import           Data.Char

main = do
  let n = read "347991"::Int
  print $ minsteps $ directions n

directions:: Int -> [Int]
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
