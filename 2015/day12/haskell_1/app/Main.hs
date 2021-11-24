module Main where

import Prelude

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Scientific

foldStructure xs = foldr (\x acc -> addNumber x + acc) 0 xs

foldStructure' xs = foldr(\x acc -> addNumber' x + acc) 0 xs

addNumber :: A.Value
          -> Scientific
addNumber (A.Number x) = x
addNumber (A.Array xs) = foldStructure xs
addNumber (A.Object xs) = foldStructure xs
addNumber _ = 0

addNumber' :: A.Value
           -> Scientific
addNumber' (A.Object xs) =
  let containsRed = (>0) . KM.size $ KM.filter (=="red") xs
   in if containsRed
         then 0
         else foldStructure' xs
addNumber' (A.Array xs) = foldStructure' xs
addNumber' (A.Number x) = x
addNumber' _ = 0

main :: IO ()
main = do
  s <- C.readFile "../input"
  let problemInput :: Maybe A.Value
      problemInput = A.decode s
      answer1 = maybe 0 (addNumber) problemInput
      answer2 = maybe 0 (addNumber') problemInput
  putStrLn $ "Part 1: " ++ show answer1
  putStrLn $ "Part 2: " ++ show answer2
