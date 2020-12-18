-- Neat and simple solution
-- https://github.com/haskelling/aoc2020/blob/main/AOC.hs
module Foo where

import Data.Char
import Text.Parsec hiding(count, parse, uncons)
import qualified Text.Parsec as Parsec
import Text.Parsec.Expr

type Parser = Parsec String ()

integer :: Parser Int
integer = read <$> many1 digit

expr = buildExpressionParser table term
term = paren <|> integer
paren = char '(' *> expr <* char ')'
table = [[Infix (char '+' >> return (+)) AssocLeft, Infix (char '*' >> return (*)) AssocLeft]]

-- table2 = [[Infix (char '+' >> return (+)) AssocLeft], [Infix (char '*' >> return (*)) AssocLeft]]


parse :: Parser a            -- ^ The parser for "a"s
      -> String              -- ^ The string to be parsed
      -> Either ParseError a -- ^ The successfully parsed value or an error
parse p = Parsec.parse p ""


parselist :: Parser a -- ^ The parser for "a"s
          -> [String] -- ^ The list of 'String's to parse
          -> [a]      -- ^ The resulting list of "a"s

parselist p = either (error . show) id . mapM (parse p)

-- main = interact' $ sum . parselist expr . lines . filter (/=' ')
