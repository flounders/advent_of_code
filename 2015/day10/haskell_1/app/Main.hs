module Main where

import Prelude

import Data.List (group)

lookSay :: String
        -> String
lookSay = foldr (\xs@(x:_) acc -> show (length xs) ++ x : acc) [] . group

solve :: String
      -> Int
solve = length . head . drop 40 . iterate (lookSay)

solve' :: String
       -> Int
solve' = length . head . drop 50 . iterate (lookSay)

main :: IO ()
main = do
  let answer1 = solve "1113122113"
      answer2 = solve' "1113122113"
  putStrLn $ "Part 1: " ++ show answer1
  putStrLn $ "Part 2: " ++ show answer2
