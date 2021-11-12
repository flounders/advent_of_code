module Main where

import Prelude

import Control.Applicative
import Data.Char (chr, isAscii, isHexDigit)
import qualified Text.ParserCombinators.ReadP as P

escapeBackslash :: P.ReadP Char
escapeBackslash = do
  P.string "\\\\"
  pure '\\'

escapeDoubleQuote :: P.ReadP Char
escapeDoubleQuote = do
  P.string "\\\""
  pure '\"'

escapeHex :: P.ReadP Char
escapeHex = do
  P.string "\\x"
  n <- P.count 2 (P.satisfy isHexDigit)
  pure . chr $ read ("0x" ++ n)

memoryString :: P.ReadP String
memoryString =
  let p = (P.many (escapeBackslash <|> escapeDoubleQuote <|> escapeHex <|> P.satisfy isAscii))
   in P.between (P.char '\"') (P.char '\"' >> P.eof) p

main :: IO ()
main = do
  s <- readFile "../input"
  let codeMemoryPairs = foldr (\x acc -> (x, fst . head $ P.readP_to_S memoryString x) : acc) [] $ lines s
      answer1 = foldr (\(c, m) acc -> (length c) - (length m) + acc) 0 codeMemoryPairs
      answer2 = foldr (\x acc -> (length $ show x) - (length x) + acc) 0 $ lines s
  putStrLn $ "Part 1: " ++ show answer1
  putStrLn $ "Part 2: " ++ show answer2
