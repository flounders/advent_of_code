module Main where

import Prelude

import Control.Applicative
import Data.Char (isDigit)
import qualified Text.ParserCombinators.ReadP as P

data Direction = Up Int
               | Down Int
               | Forward Int
               deriving (Eq, Show)

parseNumber :: P.ReadP Int
parseNumber = do
  ds <- P.munch1 isDigit
  pure $ read ds

parseDirection :: P.ReadP Direction
parseDirection =
  let parseUp = do
        P.string "up "
        Up <$> parseNumber
      parseDown = do
        P.string "down "
        Down <$> parseNumber
      parseForward = do
        P.string "forward "
        Forward <$> parseNumber
   in parseUp <|> parseDown <|> parseForward

handleDirection :: Direction
                -> (Int, Int)
                -> (Int, Int)
handleDirection d (x, y) = 
  case d of
    Up n -> (x, y - n)
    Down n -> (x, y + n)
    Forward n -> (x + n, y)

handleDirection' :: Direction
                 -> (Int, Int, Int)
                 -> (Int, Int, Int)
handleDirection' d (x, y, aim) =
  case d of
    Up n -> (x, y, aim - n)
    Down n -> (x, y, aim + n)
    Forward n -> (x + n, y + n * aim, aim)

main :: IO ()
main = do
  s <- readFile "../input"
  let directions = map (fst . head . P.readP_to_S parseDirection) $ lines s
      answer1 = uncurry (*) . foldl (flip handleDirection) (0, 0) $ directions 
      answer2 = (\(x, y, _) -> x * y) . foldl (flip handleDirection') (0, 0, 0) $ directions
  putStrLn $ "Part 1: " ++ show answer1
  putStrLn $ "Part 2: " ++ show answer2
