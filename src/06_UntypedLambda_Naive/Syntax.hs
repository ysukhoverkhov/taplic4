module Syntax (
  Term(..), justName, Name(..), Context
) where

import qualified Lexics as L

type Context = Integer

-- Data types
data Name = Name String (Maybe Context) deriving (Eq)
justName name = Name name Nothing

data Term =
  Variable Name |
  Abstraction Name Term |
  Application Term Term
  deriving (Eq)

instance Show Term  where
  show term = case term of
    Variable name ->
      show name
    Abstraction name term ->
      L.parenOpen ++ L.lambda ++ show name ++ " " ++ L.arrow ++ " "
      ++ show term ++ L.parenClose
    Application t1 t2     ->
      show t1 ++ " " ++ show  t2

instance Show Name where
  show name = case name of
    Name ident (Just context) -> ident ++ "(" ++ show context ++ ")"
    Name ident Nothing        -> ident ++ "(?)"
