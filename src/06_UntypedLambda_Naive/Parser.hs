module Parser (

) where

import           Syntax            (Name, Term (..))

import           Control.Applicative    (many, (<|>))
import           Control.Monad          (void)
import           Text.Parsec            (parse, try)
import           Text.Parsec.Char       (char, digit, letter, oneOf, string)
import           Text.Parsec.Combinator (between, chainl1, choice, many1)
import           Text.Parsec.String     (Parser)


parse :: String -> Either String Term
parse = Text.Parsec.parse term ""

-- # TODO: split syntax and semantics

-- various things
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

-- Token type
num :: Parser Integer
num = read <$> lexeme (many1 digit)

identifier :: Parser String
identifier = lexeme $ (:) <$> firstChar <*> many nonFirstChar
  where
   firstChar = letter <|> char '_'
   nonFirstChar = digit <|> firstChar

parenOpen :: Parser Char
parenOpen = lexeme $ char '('

parenClose :: Parser Char
parenClose = lexeme $ char ')'

lambda :: Parser Char
lambda = lexeme $ char '~'

arrow :: Parser String
arrow = lexeme $ string "->"

-- Parser
term :: Parser Term
term = choice [abstract, app, var, parens]

-- 0
abstract :: Parser Term
abstract = TmAbs <$> (lambda *> identifier <* arrow) <*> term

-- 1
app :: Parser Term
app = operands `chainl1` operation
  where
    operands = choice [abstract, var, parens]
    operation = return TmApp

-- 2
var :: Parser Term
var = TmVar <$> identifier

parens :: Parser Term
parens = between parenOpen parenClose term
