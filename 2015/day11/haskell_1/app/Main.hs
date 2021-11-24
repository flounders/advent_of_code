module Main where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Text (Text(..))
import qualified Data.Text as T
import qualified Data.Map.Strict as MS

incWrap :: Char
        -> (Char, Bool)
incWrap 'z' =
  ('a', True)
incWrap c =
  (succ c, False)

textElem :: Char
         -> Text
         -> Bool
textElem c s =
  maybe False (const True) $ T.find (==c) s

validate :: Text
         -> Bool
validate s =
  let hasNoForbiddenChars = not (textElem 'i' s || textElem 'o' s || textElem 'l' s)
   in hasIncSequence s && hasTwoUniquePairs s && hasNoForbiddenChars

hasIncSequence :: Text
               -> Bool
hasIncSequence s =
  (\(_, _, p) -> p) $ T.foldr (\c (a,b,p) -> if succ c == b && succ b == a
                                                then (a,b,True)
                                                else (b,c,p)) ('\0', '\0', False) s

hasTwoUniquePairs :: Text
                  -> Bool
hasTwoUniquePairs s =
  (>=2) . MS.size . MS.filter (==1) . snd $ T.foldr (\c (prev, table) -> if c == prev
                                                                            then (c, MS.insertWith (+) [c,prev] 1 table)
                                                                            else (c, table)) ('\0', MS.empty) s

iterateT :: Text
        -> Text
iterateT s =
  let last = T.last s
      init = T.init s
      (newLast, p) = incWrap last
   in if p
         then T.snoc (iterateT init) newLast
         else T.snoc init newLast

solve :: Text
      -> Text
solve = until (validate) (iterateT)

main :: IO ()
main = do
  let answer1 = solve $ T.pack "vzbxkghb"
      answer2 = solve $ iterateT answer1
  putStrLn $ "Part 1: " ++ T.unpack answer1
  putStrLn $ "Part 2: " ++ T.unpack answer2
