module Main where

import Prelude

sumThree :: [Int]
         -> [Int]
sumThree [] = []
sumThree ns =
  let three = take 3 ns
   in sum three : sumThree (tail ns)

main :: IO ()
main = do
  s <- readFile "../input"
  let ns :: [Int]
      ns = map read $ lines s
      sums = sumThree ns
      answer1 = snd $ foldl (\(p, acc) n -> if n > p
                                               then (n, acc + 1)
                                               else (n, acc)) (head ns, 0) ns
      answer2 = snd $ foldl (\(p, acc) n -> if n > p
                                               then (n, acc + 1)
                                               else (n, acc)) (head sums, 0) sums
  putStrLn $ "Part 1: " ++ show answer1
  putStrLn $ "Part 2: " ++ show answer2
