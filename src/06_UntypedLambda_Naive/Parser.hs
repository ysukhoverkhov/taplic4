module Parser (
  Parser.parse
) where

import qualified Lexics                 as L
import           Syntax                 (Name, Term (..), justName)

import           Control.Applicative    (many, (<|>))
import           Control.Monad          (void)
import           Data.Bifunctor         (first)

import           Text.Parsec            (parse, try)
import           Text.Parsec.Char       (char, digit, letter, oneOf, string)
import           Text.Parsec.Combinator (between, chainl1, choice, many1)
import           Text.Parsec.Error      (ParseError (..))
import           Text.Parsec.String     (Parser)


parse :: String -> Either String Term
parse string = first transformParserError $ Text.Parsec.parse term "" string
  where
    transformParserError = show

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

parenOpen :: Parser String
parenOpen = lexeme $ string L.parenOpen

parenClose :: Parser String
parenClose = lexeme $ string L.parenClose

lambda :: Parser String
lambda =  lexeme $ string L.lambda

arrow :: Parser String
arrow = lexeme $ string L.arrow

-- Parser
term :: Parser Term
term = choice [abstract, app, var, parens]

-- 0
abstract :: Parser Term
abstract = Abstraction <$> (lambda *> (justName <$> identifier) <* arrow) <*> term

-- 1
app :: Parser Term
app = operands `chainl1` operation
  where
    operands = choice [abstract, var, parens]
    operation = return Application

-- 2
var :: Parser Term
var = Variable . justName <$> identifier

parens :: Parser Term
parens = between parenOpen parenClose term
