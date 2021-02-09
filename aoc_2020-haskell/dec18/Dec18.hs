{-# LANGUAGE FlexibleContexts #-}
-- YAY
-- So many ways to do this.
-- https://www.reddit.com/r/adventofcode/comments/kfeldk/2020_day_18_solutions/gg942y2/
module Dec18 where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)


-- this "language" looks like haskell
lexer      = P.makeTokenParser haskellDef


-- tokens
parens     = P.parens lexer
natural    = P.natural lexer
reservedOp = P.reservedOp lexer


-- terms in our expression are simple

term e  =  parens e
        <|> natural
        <?> "simple expression"


-- two tables, for part 1 and 2

table1  = [ [binary "+" (+) AssocLeft, binary "*" (*) AssocLeft ] ]

table2  = [ [binary "+" (+) AssocLeft],
            [binary "*" (*) AssocLeft] ]


-- and shared binary op implementation. We eval as we parse ;)
binary name fun = Infix (do { reservedOp name; return fun })


-- two kinds of expression parser

expr1   = buildExpressionParser table1 (term expr1)
        <?> "part 1 expression"

expr2   = buildExpressionParser table2 (term expr2)
        <?> "part 2 expression"


--

parseString expr str = case parse expr "" str of
  Left e  -> error $ show e
  Right r -> r


solve1 = sum . map (parseString expr1)
solve2 = sum . map (parseString expr2)


main = do
  dat <- readFile "data.txt"
  let input = lines dat

  print $ solve1 input
  print $ solve2 input
