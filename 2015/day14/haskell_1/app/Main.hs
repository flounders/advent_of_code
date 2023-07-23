{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Prelude

import Control.Monad (replicateM)
import Control.Monad.Trans.State.Lazy
import Data.Char (isDigit)
import qualified Data.Map.Strict as MS

newtype Seconds = Seconds Int
  deriving (Eq, Num, Show)

newtype Velocity = Velocity Int
  deriving (Eq, Num, Show)

newtype Rest = Rest Seconds
  deriving (Eq, Num, Show)

data Move = Move { velocity :: Velocity
                 , duration :: Seconds
                 }
                 deriving Show

data Reindeer = Reindeer { name :: String
                         , move :: Move
                         , rest :: Rest
                         } deriving Show

parseReindeer :: String -> Reindeer
parseReindeer s =
  let ws = words s
      n = head ws
      isNumber (x:_) = isDigit x
      isNumber _ = False
      numbers = map read . take 3 $ filter isNumber ws
      createReindeer (v:d:r:_) = Reindeer n (Move (Velocity v) (Seconds d)) (Rest (Seconds r))
      createReindeer _ = error "Unable to parse"
   in createReindeer numbers

descriptionToReindeer :: String -> MS.Map String Reindeer
descriptionToReindeer =
  foldr ((\x acc -> MS.insert (name x) x acc) . parseReindeer) MS.empty . lines

exampleDescription :: String
exampleDescription =
  "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\n\
  \Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."

distanceTravelled :: Reindeer
                  -> Int
                  -> Int
distanceTravelled r secs =
  let totalDuration = restTime + durationTime
      fromSeconds (Seconds s) = s
      restTime = fromSeconds $ (\(Rest x) -> x) (rest r)
      durationTime = fromSeconds . duration $ move r
      v = (\(Velocity x) -> x) . velocity $ move r
      (cycles, remSecs') = secs `divMod` totalDuration
      remSecs = if (remSecs' - durationTime) < 0
                   then remSecs'
                   else durationTime
   in cycles * durationTime * v + remSecs * v

leaders :: MS.Map String Int
        -> [String]
leaders rs =
  let maxDistance = maximum $ MS.elems rs
      ls = MS.filter (== maxDistance) rs
   in MS.keys ls

scoring :: MS.Map String Reindeer
        -> Int
        -> Int
scoring rs seconds =
  maximum . snd $ execState (replicateM (seconds - 1) $ do
    (t, scores) <- get
    let scores' = foldr (\x acc -> MS.insertWith (+) x 1 acc) scores . leaders $ MS.map (`distanceTravelled` t) rs
    put (t+1, scores')) (1, MS.empty)

main :: IO ()
main = do
  s <- readFile "../input"
  let referenceReindeer = descriptionToReindeer s
      exampleReferenceReindeer = descriptionToReindeer exampleDescription
      exampleAnswer1 = maximum . MS.elems $ MS.map (`distanceTravelled` 1000) exampleReferenceReindeer
      exampleAnswer2 = scoring exampleReferenceReindeer 1000
      answer1 = maximum . MS.elems $ MS.map (`distanceTravelled` 2503) referenceReindeer
      answer2 = scoring referenceReindeer 2503
  putStrLn $ "Example 1: " ++ show exampleAnswer1
  putStrLn $ "Example 2: " ++ show exampleAnswer2
  putStrLn $ "Answer 1: " ++ show answer1
  putStrLn $ "Answer 2: " ++ show answer2
