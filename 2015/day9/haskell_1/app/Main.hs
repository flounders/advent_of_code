module Main where

import Prelude

import qualified Data.Graph.Types as GT
import qualified Data.Graph.UGraph as UG
import qualified Text.ParserCombinators.ReadP as P

parseRoute :: P.ReadP (GT.Edge String Int)
parseRoute = do
  origin <- P.manyTill (P.satisfy $ const True) (P.string " to ")
  destination <- P.manyTill (P.satisfy $ const True) (P.string " = ")
  distance <- P.munch1 $ const True
  pure $ GT.Edge origin destination (read distance)

shortest :: UG.UGraph String Int
shortest =
  undefined

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
