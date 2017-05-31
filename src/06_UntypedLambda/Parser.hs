module Parser where

import Data.Char

import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar, satisfy, space, char, string, spaces, digit, oneOf, letter)
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

-- TODO: seporate parsing/lexing concerns completelly.

-- various things
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

-- Token type
num :: Parser SimpleExpr
num = Num . read <$> lexeme (many1 digit)

var :: Parser SimpleExpr
var =
  Var <$> iden
  where
   iden = lexeme $ (:) <$> firstChar <*> many nonFirstChar
   firstChar = letter <|> char '_'
   nonFirstChar = digit <|> firstChar

parens :: Parser SimpleExpr
parens = Parens <$> (lexeme (char '(') *> simpleExpr <* lexeme (char ')'))

operator :: Char -> (SimpleExpr -> SimpleExpr -> SimpleExpr) -> Parser (SimpleExpr -> SimpleExpr -> SimpleExpr)
operator c op = lexeme (char c) *> return op

addOperator :: Parser (SimpleExpr -> SimpleExpr -> SimpleExpr)
addOperator = operator '+' Add

subOperator :: Parser (SimpleExpr -> SimpleExpr -> SimpleExpr)
subOperator = operator '-' Sub

mulOperator :: Parser (SimpleExpr -> SimpleExpr -> SimpleExpr)
mulOperator = operator '*' Mul

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
