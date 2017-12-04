module Main where
import Data.List

main :: IO ()
main = do
  ls <- lines <$> readFile  "inputs/input2.txt"
  let is = map (map (\ s -> read s :: Int) . words) ls
  print $ checksum1 is
  print $ checksum2 is


checksum1 = sum . map (\l -> maximum l - minimum l)

checksum2 = sum . map (\l -> head [ x `div` y | x <- l, y <- l, x/=y, x `mod` y == 0])
