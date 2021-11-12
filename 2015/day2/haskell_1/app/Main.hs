module Main where

import Prelude
import Data.Char as C
import Data.List (sort)
import Text.ParserCombinators.ReadP as P

parseDimensions :: P.ReadP (Int,Int,Int)
parseDimensions = do
  let parseNumber = P.munch1 C.isDigit
  l <- parseNumber
  P.char 'x'
  w <- parseNumber
  P.char 'x'
  h <- parseNumber
  pure $ (read l, read w, read h)

wrapEstimate :: (Int, Int, Int)
             -> Int
wrapEstimate (l, w, h) =
  let lw = l * w
      wh = w * h
      hl = h * l
      minSurface = foldr min lw [lw,wh,hl]
   in 2 * lw + 2 * wh + 2 * hl + minSurface

ribbonEstimate :: (Int, Int, Int)
               -> Int
ribbonEstimate (l, w, h) =
  let sortedDims = sort [l, w, h]
      min1 = head sortedDims
      min2 = head $ drop 1 sortedDims
   in min1 + min1 + min2 + min2 + l * w * h

solvePart1 :: String
           -> Int
solvePart1 = foldr (\x acc -> (wrapEstimate . fst . head $ P.readP_to_S parseDimensions x) + acc) 0 . lines

solvePart2 :: String
           -> Int
solvePart2 = foldr (\x acc -> (ribbonEstimate . fst . head $ P.readP_to_S parseDimensions x) + acc) 0 . lines

main :: IO ()
main = do
  s <- readFile "../input"
  let answer1 = solvePart1 s
  let answer2 = solvePart2 s
  putStrLn ("Part 1: " ++ show answer1)
  putStrLn ("Part 2: " ++ show answer2)
