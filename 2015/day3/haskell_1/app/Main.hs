module Main where

import Prelude

import qualified Data.Map.Strict as MS

data Delivery = Delivery { turn :: Person
                         , position :: MS.Map Person (Int, Int)
                         , houses :: MS.Map (Int, Int) Int
                         }
                         deriving Show

data Person = Santa
            | Robot
            deriving (Eq, Ord, Show)

-- Part 1
origin :: MS.Map (Int, Int) Int
origin = MS.singleton (0,0) 1

-- Part 2 for Santa and RoboSanta
houseOrigin :: MS.Map (Int, Int) Int
houseOrigin = MS.singleton (0,0) 2

deliveryOrigin :: Delivery
deliveryOrigin = Delivery Santa (MS.fromList [(Santa, (0,0)), (Robot, (0,0))]) houseOrigin

newCoordinates :: Char
               -> (Int, Int)
               -> (Int, Int)
newCoordinates d (x,y) = case d of
                          '^' -> (x, y+1)
                          'v' -> (x, y-1)
                          '<' -> (x-1, y)
                          '>' -> (x+1, y)

takeTurn :: Char
         -> Delivery
         -> Delivery
takeTurn dir del =
  let pos = (position del) MS.! (turn del)
      newC = newCoordinates dir pos
      newP = MS.insert (turn del) newC (position del)
      newHouses = MS.insertWith (+) newC 1 (houses del)
      newPerson = changePerson $ turn del

      changePerson :: Person -> Person
      changePerson Santa = Robot
      changePerson Robot = Santa

   in del{turn=newPerson, position=newP, houses=newHouses}

solvePart1 :: String -> Int
solvePart1 s = 
  let deliveries = foldr (\x (c,m) -> let newC = newCoordinates x c
                                       in (newC, MS.insertWith (+) newC 1 m)) ((0,0), origin) s
   in MS.size $ snd deliveries

solvePart2 :: String -> Int
solvePart2 s =
  let deliveries = foldl (flip takeTurn) deliveryOrigin s
   in MS.size $ houses deliveries

main :: IO ()
main = do
  s <- readFile "../input"
  let answer1 = solvePart1 s
  let answer2 = solvePart2 s
  putStrLn ("Part 1: " ++ show answer1)
  putStrLn ("Part 2: " ++ show answer2)
