module Main where

import Prelude

import Data.List (transpose)

binary :: [Char]
       -> Int
binary ds =
  let charToInt '1' = 1
      charToInt _ = 0
   in foldl (\acc x -> acc * 2 + charToInt x) 0 ds

mostCommonValue :: [Char] 
                -> Char
mostCommonValue bs =
  let onesCount = length $ filter (=='1') bs
      zerosCount = length $ filter (=='0') bs
   in if onesCount > zerosCount
         then '1'
         else '0'

g :: (Int -> Int -> Char)
  -> [String]
  -> [String]
  -> Int
  -> String
g p ns ts i
  | length ns == 1 = drop i $ head ns
  | i < length ts =
  let onesCount = filterForBit '1'
      zerosCount = filterForBit '0'
      filterForBit c = length . filter (==c) $ ts !! i
      filterNumbers c = filter (\x -> (x !! i) == c) ns
      retainedValue = p onesCount zerosCount
      ns' = filterNumbers retainedValue
   in retainedValue : g p ns' (transpose ns') (i+1)
  | otherwise = []

flipBit :: Char
        -> Char
flipBit '1' = '0'
flipBit '0' = '1'
flipBit c = c

main :: IO ()
main = do
  s <- readFile "../input"
  let transposed = transpose $ lines s
      gamma1 = map mostCommonValue transposed
      epsilon1 = map flipBit gamma1
      answer1 = binary gamma1 * binary epsilon1
      oxyRating = binary $ g (\x y -> if x >= y
                                         then '1'
                                         else '0') (lines s) transposed 0
      co2Rating = binary $ g (\x y -> if x >= y
                                         then '0'
                                         else '1') (lines s) transposed 0
      answer2 = oxyRating * co2Rating
  putStrLn $ "Part 1: " ++ show answer1
  putStrLn $ "Part 2: " ++ show answer2
