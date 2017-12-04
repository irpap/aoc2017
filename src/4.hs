module Main where

import Data.List

main :: IO ()
main = do
  xs <- (map words . lines) <$> readFile  "inputs/input4.txt"
  print $ (length . filter validPassword1) xs
  print $ (length . filter validPassword2) xs

validPassword1::[String] -> Bool
validPassword1 p = all (\x -> notElem x (delete x p)) p

validPassword2::[String] -> Bool
validPassword2 p = validPassword1 (map sort p)
