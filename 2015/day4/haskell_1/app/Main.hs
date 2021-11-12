module Main where

import Prelude

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base16 as B
import qualified Crypto.Hash.MD5 as M

findLowestPositive :: C.ByteString -> Int
findLowestPositive secret = untilB validate (B.encode . M.hash . C.append secret . C.pack . show) (+1) 1

findLowestPositive' :: C.ByteString -> Int
findLowestPositive' secret = untilB validatePart2 (B.encode . M.hash . C.append secret . C.pack . show) (+1) 1

untilB :: (b -> Bool) -> (a -> b) -> (a -> a) -> a -> a
untilB p f g x
  | p (f x) = x
  | otherwise = untilB p f g (g x)

validate :: C.ByteString -> Bool
validate = and . map (\x -> x == '0') . take 5 . C.unpack

validatePart2 :: C.ByteString -> Bool
validatePart2 = and . map (\x -> x == '0') . take 6 . C.unpack

main :: IO ()
main = do
  putStrLn ("Part 1: " ++ show (findLowestPositive $ C.pack "iwrupvqb"))
  putStrLn ("Part 2: " ++ show (findLowestPositive' $ C.pack "iwrupvqb"))
