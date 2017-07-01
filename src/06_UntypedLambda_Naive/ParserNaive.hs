module ParserNaive where

import           Data.Char

import           SyntaxNaive            (Name, Term (..))

import           Control.Applicative    (many, (<|>))
import           Control.Monad          (void)
import           Text.Parsec            (parse, try)
import           Text.Parsec.Char       (anyChar, char, digit, letter, oneOf,
                                         satisfy, space, spaces, string)
import           Text.Parsec.Combinator (chainl1, many1)
import           Text.Parsec.String     (Parser)

-- various things
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

-- Token type
num :: Parser Integer
num = read <$> lexeme (many1 digit)

str :: Parser String
str = lexeme $ (:) <$> firstChar <*> many nonFirstChar
  where
   firstChar = letter <|> char '_'
   nonFirstChar = digit <|> firstChar

parenOpen :: Parser Char
parenOpen = lexeme $ char '('

parenClose :: Parser Char
parenClose = lexeme $ char ')'

-- TODO: name it correctly.
lambda :: Parser Char
lambda = lexeme $ char '~'

-- TODO: make good name.
arrow :: Parser String
arrow = lexeme $ string "->"

-- Parser
name :: Parser Name
name = str

var :: Parser Term
var = TmVar <$> name

-- TODO: replace `var` here with `term`
abstr :: Parser Term
abstr =  (TmAbs <$> (parenOpen *> lambda *> name <* arrow)) <*> (var <* parenClose)

-- TODO: replace `var` here with `term`
app :: Parser Term
app =  (TmApp <$> var) <*> var

term :: Parser Term
term = try abstr <|> try app <|> var



-- parens :: Parser SimpleExpr
-- parens = Parens <$> (lexeme (char '(') *> simpleExpr <* lexeme (char ')'))
--
-- operator :: Char -> (SimpleExpr -> SimpleExpr -> SimpleExpr) -> Parser (SimpleExpr -> SimpleExpr -> SimpleExpr)
-- operator c op = lexeme (char c) *> return op
--
-- addOperator :: Parser (SimpleExpr -> SimpleExpr -> SimpleExpr)
-- addOperator = operator '+' Add
--
-- subOperator :: Parser (SimpleExpr -> SimpleExpr -> SimpleExpr)
-- subOperator = operator '-' Sub
--
-- mulOperator :: Parser (SimpleExpr -> SimpleExpr -> SimpleExpr)
-- mulOperator = operator '*' Mul
--
-- simpleExpr :: Parser SimpleExpr
-- simpleExpr = term `chainl1` op
--   where
--     term = parens <|> num <|> var
--     op = addOperator <|> subOperator <|> mulOperator
--
-- -- Me here is just for fun
--
-- isNum :: SimpleExpr -> Bool
-- isNum e = case e of
--   Num _ -> True
--   _     -> False
--
-- eval :: SimpleExpr -> SimpleExpr
-- eval e = case e of
--   Add (Num n1) (Num n2) -> Num (n1 + n2)
--   Add e1 e2 | not (isNum e1) -> Add (eval e1) e2
--             | otherwise -> Add e1 (eval e2)
--   Sub (Num n1) (Num n2) -> Num (n1 - n2)
--   Sub e1 e2 | not (isNum e1) -> Sub (eval e1) e2
--             | otherwise -> Sub e1 (eval e2)
--   Mul (Num n1) (Num n2) -> Num (n1 * n2)
--   Mul e1 e2 | not (isNum e1) -> Mul (eval e1) e2
--             | otherwise -> Mul e1 (eval e2)
--
-- evalAll :: SimpleExpr -> SimpleExpr
-- evalAll e | isNum e = e
--           | otherwise = evalAll $ eval e
