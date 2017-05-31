module Parser where


import Data.Char

import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar, satisfy, space, char, string, spaces, digit, oneOf)
import Text.Parsec.Combinator (many1, chainl1)
import Text.Parsec (parse, try)
import Control.Applicative ((<|>), many)
import Control.Monad (void)


-- test =
test = parse simpleExpr "" "1+0*3"


-- syntax
data SimpleExpr = Num Integer
                 | Var String
                 | Add SimpleExpr SimpleExpr
                 | Sub SimpleExpr SimpleExpr
                 | Mul SimpleExpr SimpleExpr
                 | Parens SimpleExpr
                   deriving (Eq,Show)

-- various things
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
 x <- p
 whitespace
 return x

-- Token type
num :: Parser SimpleExpr
num = do
 n <- lexeme $ many1 digit
 return $ Num $ read n

var :: Parser SimpleExpr
var = do
   fc <- firstChar
   rest <- lexeme $ many nonFirstChar
   return $ Var (fc:rest)
 where
   firstChar = satisfy (\a -> isLetter a || a == '_')
   nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

parens :: Parser SimpleExpr
parens = do
   void $ lexeme $ char '('
   e <- simpleExpr
   void $ lexeme $ char ')'
   return $ Parens e

addOperator :: Parser (SimpleExpr -> SimpleExpr -> SimpleExpr)
addOperator = do
   void $ lexeme $ char '+'
   return Add

subOperator :: Parser (SimpleExpr -> SimpleExpr -> SimpleExpr)
subOperator = do
  void $ lexeme $ char '-'
  return Sub

mulOperator :: Parser (SimpleExpr -> SimpleExpr -> SimpleExpr)
mulOperator = do
  void $ lexeme $ char '*'
  return Mul

simpleExpr :: Parser SimpleExpr
simpleExpr = term `chainl1` op
  where
    term = parens <|> num <|> var
    op = addOperator <|> subOperator <|> mulOperator

-- Me here is just for fun

isNum :: SimpleExpr -> Bool
isNum e = case e of
  Num _ -> True
  _ -> False

eval :: SimpleExpr -> SimpleExpr
eval e = case e of
  Add (Num n1) (Num n2) -> Num (n1 + n2)
  Add e1 e2 | not (isNum e1) -> Add (eval e1) e2
            | otherwise -> Add e1 (eval e2)
  Sub (Num n1) (Num n2) -> Num (n1 - n2)
  Sub e1 e2 | not (isNum e1) -> Sub (eval e1) e2
            | otherwise -> Sub e1 (eval e2)
  Mul (Num n1) (Num n2) -> Num (n1 * n2)
  Mul e1 e2 | not (isNum e1) -> Mul (eval e1) e2
            | otherwise -> Mul e1 (eval e2)

evalAll :: SimpleExpr -> SimpleExpr
evalAll e | isNum e = e
          | otherwise = evalAll $ eval e
