module Main where

import Prelude

import qualified Data.Graph.Types as GT
import qualified Data.Graph.UGraph as UG
import Data.List (minimum, notElem, uncons)
import qualified Text.ParserCombinators.ReadP as P

parseRoute :: P.ReadP (GT.Edge String Int)
parseRoute = do
  origin <- P.manyTill (P.satisfy $ const True) (P.string " to ")
  destination <- P.manyTill (P.satisfy $ const True) (P.string " = ")
  distance <- P.munch1 $ const True
  pure $ GT.Edge origin destination (read distance)

findOrd :: (Int -> Int -> Bool)
        -> UG.UGraph String Int
        -> [String]
        -> String
        -> (String, Int)
findOrd f g visited origin =
  let ies = UG.incidentEdges g origin
      unvisitedEdges = filter (\(GT.Edge x y _) -> (x `notElem` visited) || (y `notElem` visited)) ies
      shortestEdge = foldr1 (\x@(GT.Edge _ _ d) acc@(GT.Edge _ _ acc_d) -> if f d acc_d
                                                                              then x
                                                                              else acc) unvisitedEdges
      (newVisited, distance) = (\(GT.Edge x y d) -> if x /= origin
                                                       then (x, d)
                                                       else (y, d)) shortestEdge
   in (newVisited, distance)

shortest :: UG.UGraph String Int
         -> [String]
         -> String
         -> (String, Int)
shortest = findOrd (<)

longest :: UG.UGraph String Int
        -> [String]
        -> String
        -> (String, Int)
longest = findOrd (>)

testS :: String
testS =
  "London to Dublin = 464\n\
  \London to Belfast = 518\n\
  \Dublin to Belfast = 141"

testGraph :: UG.UGraph String Int
testGraph =
  UG.fromEdgesList . map (fst . head . P.readP_to_S parseRoute) $ lines testS

solve :: (UG.UGraph String Int -> [String] -> String -> (String, Int))
      -> UG.UGraph String Int
      -> [String]
      -> Int
      -> Int
solve h g visited distance 
  | GT.order g == length visited = distance
  | otherwise = 
    let f (town, d) = solve h g (town : visited) (d + distance)
     in case uncons visited of
          Nothing -> solve h g [head $ GT.vertices g] distance
          Just (x,_) -> f $ h g visited x

main :: IO ()
main = do
  s <- readFile "../input"
  let fileGraph = UG.fromEdgesList . map (fst . head . P.readP_to_S parseRoute) $ lines s
      vs = GT.vertices fileGraph
      answer1 = minimum $ map (\x -> solve shortest fileGraph [x] 0) vs
      answer2 = maximum $ map (\x -> solve longest fileGraph [x] 0) vs
  putStrLn $ "Part 1: " ++ show answer1
  putStrLn $ "Part 2: " ++ show answer2
