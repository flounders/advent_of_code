module Main where

import Prelude

-- Part 1
floorNumber :: String -> Int
floorNumber = foldr (\x acc -> acc + floorDirection x) 0

floorDirection :: Char -> Int
floorDirection c = case c of
  '(' -> 1
  ')' -> -1

-- Part 2
basementStep :: String -> Int
basementStep = length . takeWhile (>=0) . scanl (\acc x -> acc + floorDirection x) 0

main :: IO ()
main = do
  s <- readFile "../input"
  let answer1 = floorNumber s
      answer2 = basementStep s
  putStrLn $ "Part 1: " ++ show answer1
  putStrLn $ "Part 2: " ++ show answer2
