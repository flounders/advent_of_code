module Main where

import Prelude

import Data.Char (isDigit)
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as MS
import qualified Text.ParserCombinators.ReadP as P

data Command = On
             | Off
             | Toggle
             deriving (Eq, Show)

type Start = (Int, Int)
type End = (Int, Int)

parseCommand :: P.ReadP Command
parseCommand =
  let parseOn = do
        P.string "turn on"
        pure On
      parseOff = do
        P.string "turn off"
        pure Off
      parseToggle = do
        P.string "toggle"
        pure Toggle
   in parseOn P.+++ parseOff P.+++ parseToggle

parsePair :: P.ReadP (Int, Int)
parsePair = do
  let parseNumber = do
        digits <- P.munch1 isDigit
        pure $ read digits
  x <- parseNumber
  P.char ','
  y <- parseNumber
  pure (x, y)

parseInstruction :: P.ReadP (Command, Start, End)
parseInstruction = do
  command <- parseCommand
  P.char ' '
  start <- parsePair
  P.string " through "
  end <- parsePair
  pure (command, start, end)

runInstruction :: (Command, Start, End)
               -> MS.HashMap Int Bool
               -> MS.HashMap Int Bool
runInstruction (c, (sx,sy), (ex,ey)) m =
  let range = [ x + y * 1000 | x <- [sx .. ex], y <- [sy .. ey] ]
   in case c of
        On -> foldl' (\acc x -> MS.insert x True acc) m range
        Off -> foldl' (\acc x -> MS.insert x False acc) m range
        Toggle -> foldl' (\acc x -> MS.insertWith (\_ x -> not x) x True acc) m range

runInstruction' :: (Command, Start, End)
                -> MS.HashMap Int Int
                -> MS.HashMap Int Int
runInstruction' (c, (sx,sy), (ex,ey)) m =
  let range = [ x + y * 1000 | x <- [sx .. ex], y <- [sy .. ey] ]
   in case c of
        On -> foldl' (\acc x -> MS.insertWith (+) x 1 acc) m range
        Off -> foldl' (\acc x -> MS.insertWith (\_ z -> if z == 0
                                                           then 0
                                                           else z - 1) x 0 acc) m range
        Toggle -> foldl' (\acc x -> MS.insertWith (+) x 2 acc) m range

main :: IO ()
main = do
  s <- readFile "../input"
  let instructions = map fst $ foldr (\x acc -> (P.readP_to_S parseInstruction x) ++ acc) [] (lines s)
      --answer1 = MS.size . MS.filter id $ foldl' (flip runInstruction) MS.empty instructions
      answer2 = MS.foldl' (+) 0 $ foldl' (flip runInstruction') MS.empty instructions
  --putStrLn $ ("Part 1: " ++ show answer1)
  putStrLn $ ("Part 2: " ++ show answer2)
