module Main where

import Data.List (sort)

parseInput :: String
           -> [[Int]]
parseInput s =
  let f "" (working, acc) = ([], working : acc)
      f x (working, acc) = (read x : working, acc)
   in snd . foldr f ([], []) $ lines s

solve1 :: String -> Int
solve1 s = maximum . map sum $ parseInput s

solve2 :: String -> Int
solve2 s = sum . take 3 . reverse . sort . map sum $ parseInput s

main :: IO ()
main = do
  example <- readFile "../example"
  putStrLn $ "Part 1 on Example: " ++ show (solve1 example)
  input <- readFile "../input"
  putStrLn $ "Part 1 on Input: " ++ show (solve1 input)
  putStrLn $ "Part 2 on Example: " ++ show (solve2 example)
  putStrLn $ "Part 2 on Input: " ++ show (solve2 input)
