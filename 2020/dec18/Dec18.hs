-- YAY
module Dec18 where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)


lexer      = P.makeTokenParser haskellDef

-- tokens
parens     = P.parens lexer
natural    = P.natural lexer
reservedOp = P.reservedOp lexer


-- data Exp =
--       EAdd Exp Exp
--     | ESub Exp Exp
--     | EMul Exp Exp
--     | EDiv Exp Exp
--     | EInt Integer


expr    = buildExpressionParser table term
        <?> "expression"

term    =  parens expr
        <|> natural
        <?> "simple expression"

table   = [ [binary "+" (+) AssocLeft, binary "*" (*) AssocLeft ] ]

binary  name fun assoc = Infix (do { reservedOp name; return fun }) assoc


evalString :: String -> Integer
evalString str = case parse expr "" str of
  Left e  -> error $ show e
  Right r -> r


solve1 = sum . map evalString


main = do
  dat <- readFile "data.txt"
  let input = lines dat
  -- mapM_ (print . evalString) input

  print $ solve1 input
