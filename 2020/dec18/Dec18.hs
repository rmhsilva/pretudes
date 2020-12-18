{-# LANGUAGE FlexibleContexts #-}
-- YAY
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


-- expr     = buildExpressionParser table expr

expr1    = buildExpressionParser table1 (term expr1)
        <?> "expression"

table1   = [ [binary "+" (+) AssocLeft, binary "*" (*) AssocLeft ] ]

binary  name fun = Infix (do { reservedOp name; return fun })


term e  =  parens e
        <|> natural
        <?> "simple expression"


expr2   = buildExpressionParser table2 (term expr2)
        <?> "expression"


table2   = [ [binary "+" (+) AssocLeft],
             [binary "*" (*) AssocLeft] ]



--

evalString expr str = case parse expr "" str of
  Left e  -> error $ show e
  Right r -> r


solve1 = sum . map (evalString expr1)
solve2 = sum . map (evalString expr2)


main = do
  dat <- readFile "data.txt"
  let input = lines dat

  print $ solve1 input
  print $ solve2 input
