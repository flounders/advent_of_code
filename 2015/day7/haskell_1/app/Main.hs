module Main where

import Prelude

import Control.Applicative ((<|>))
import Data.Bits
import Data.Char (isDigit, isLower)
import Data.Maybe (fromJust)
import Data.Word (Word16)
import qualified Data.Map.Strict as MS
import qualified Text.ParserCombinators.ReadP as P

data Term = Constant Word16
          | Wire String
          | Not Term
          | And Term Term
          | Or Term Term
          | LShift Term Term
          | RShift Term Term
          deriving (Eq, Ord, Show)

newtype Source = Source Term deriving Show
newtype Destination = Destination Term deriving Show

parseMapping :: P.ReadP (Source, Destination)
parseMapping = do
  let parseDestination = do
        t <- parseTerm
        pure $ Destination t
      parseSource = do
        t <- parseTerm
        pure $ Source t
  src <- parseSource
  P.string " -> "
  dst <- parseDestination
  pure (src, dst)

parseTerm :: P.ReadP Term
parseTerm = do
  let parseConstant = do
        ds <- P.munch1 isDigit
        pure . Constant $ read ds
      parseWire = do
        s <- P.munch1 isLower
        pure $ Wire s
      parseNot = do
        P.string "NOT "
        t <- parseWire <|> parseConstant
        pure $ Not t
      parseAnd = do
        t1 <- parseWire <|> parseConstant
        P.string " AND "
        t2 <- parseWire <|> parseConstant
        pure $ And t1 t2
      parseOr = do
        t1 <- parseWire <|> parseConstant
        P.string " OR "
        t2 <- parseWire <|> parseConstant
        pure $ Or t1 t2
      parseLShift = do
        t1 <- parseWire <|> parseConstant
        P.string " LSHIFT "
        t2 <- parseWire <|> parseConstant
        pure $ LShift t1 t2
      parseRShift = do
        t1 <- parseWire <|> parseConstant
        P.string " RSHIFT "
        t2 <- parseWire <|> parseConstant
        pure $ RShift t1 t2
  parseRShift <|> parseLShift <|> parseOr <|> parseAnd <|> parseNot <|> parseWire <|> parseConstant

simplify :: Term
         -> MS.Map Term Term
         -> Maybe (Term, MS.Map Term Term)
simplify t@(Constant _) m = Just (t, m)
simplify t@(Wire _) m = do
  x <- MS.lookup t m
  case x of
    (Constant y) -> pure (Constant y, m)
    z -> case simplify z m of
      Nothing -> Nothing
      Just (c, m') -> pure (c, MS.insert t c m')
simplify t@(Not x) m = do
  (Constant y, m') <- simplify x m
  pure (Constant $ complement y, m')
simplify t@(And x y) m = do
  (Constant x', m') <- simplify x m
  (Constant y', m'') <- simplify y m'
  pure $ (Constant $ x' .&. y', m'')
simplify t@(Or x y) m = do
  (Constant x', m') <- simplify x m
  (Constant y', m'') <- simplify y m'
  pure $ (Constant $ x' .|. y', m'')
simplify t@(LShift x y) m = do
  (Constant x', m') <- simplify x m
  (Constant y', m'') <- simplify y m'
  pure $ (Constant $ shiftL x' (fromIntegral y'), m'')
simplify t@(RShift x y) m = do
  (Constant x', m') <- simplify x m
  (Constant y', m'') <- simplify y m'
  pure $ (Constant $ shiftR x' (fromIntegral y'), m'')

testS :: String
testS =
  "123 -> x\n\
\456 -> y\n\
\x AND y -> d\n\
\x OR y -> e\n\
\x LSHIFT 2 -> f\n\
\y RSHIFT 2 -> g\n\
\NOT x -> h\n\
\NOT y -> i"

testMappings :: [(Source, Destination)]
testMappings = foldr (\x acc -> (fst . head $ P.readP_to_S parseMapping x) : acc) [] $ lines testS

main :: IO ()
main = do
  s <- readFile "../input"
  let textMappings = foldr (\x acc -> (fst . head $ P.readP_to_S parseMapping x) : acc) [] $ lines s
      mappings = foldr (\(Source s, Destination d) acc -> MS.insert d s acc) MS.empty textMappings
      answer1 = fst . fromJust $ simplify (Wire "a") mappings
      mappings' = MS.insert (Wire "b") answer1 mappings
      answer2 = fst . fromJust $ simplify (Wire "a") mappings'
  putStrLn $ ("Part 1: " ++ show answer1)
  putStrLn $ ("Part 2: " ++ show answer2)
