{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude

import qualified Data.Map.Strict as MS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

validate :: T.Text -> Bool
validate s =
  let sMap = T.foldr (\x acc -> MS.insertWith (+) x 1 acc) MS.empty s
   in and [niceVowels sMap, niceDoubles s, noNaughtySubs s]

validate' :: T.Text -> Bool
validate' s = repeatingSep s && pairNoOverlap s

niceVowels :: MS.Map Char Int -> Bool
niceVowels m = (>=3) $ foldr (\x acc -> (fromMaybe 0 $ MS.lookup x m) + acc) 0 ['a', 'e', 'i', 'o', 'u']

niceDoubles :: T.Text -> Bool
niceDoubles = snd . T.foldr (\x (prevX, p) -> if x == prevX
                                                   then (x, True)
                                                   else (x, p)) ('\0', False)

noNaughtySubs :: T.Text -> Bool
noNaughtySubs s = not $ or [ T.isInfixOf "ab" s
                           , T.isInfixOf "cd" s
                           , T.isInfixOf "pq" s
                           , T.isInfixOf "xy" s
                           ]

repeatingSep :: T.Text -> Bool
repeatingSep =
  (\(_, _, x) -> x) . T.foldr (\x (y,z,p) -> if x == z
                                                then (x, y, True)
                                                else (x, y, p)) ('\0', '\0', False)

pairNoOverlap :: T.Text -> Bool
pairNoOverlap s =
  let candidates = MS.filter (>=2) . snd $ T.foldr (\x (y, acc) -> (x, MS.insertWith (+) (x,y) 1 acc)) ('\0', MS.empty) s
      possibleOverlap = MS.foldrWithKey (\(x,y) _ acc -> if x == y
                                                            then True
                                                            else acc) False candidates
      noOverlaps = if possibleOverlap
                      then MS.foldrWithKey (\(x,_) _ acc -> if noOverlap x s
                                                               then True
                                                               else acc) False candidates
                      else True
   in (not $ MS.null candidates) && noOverlaps

noOverlap :: Char -> T.Text -> Bool
noOverlap x s =
  let firstMatch = T.take 2 . snd $ firstBreak
      firstBreak = T.breakOn matchString s
      secondMatch = T.take 2 . snd $ secondBreak
      secondBreak = T.breakOn matchString . T.drop 2 . snd $ firstBreak
      matchString = T.pack [x,x]
   in firstMatch == secondMatch

main :: IO ()
main = do
  s <- readFile "../input"
  let answer1 = length . filter id . map (validate . T.pack) $ lines s
  let answer2 = length . filter id . map (validate' . T.pack) $ lines s
  putStrLn ("Part 1: " ++ show answer1)
  putStrLn ("Part 2: " ++ show answer2)
