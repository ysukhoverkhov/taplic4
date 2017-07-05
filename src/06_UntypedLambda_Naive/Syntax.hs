module Syntax (
  Term(..), Name
) where

import qualified Lexics as L

-- Data types
type Name = String

data Term =
  Variable Name |
  Abstraction Name Term |
  Application Term Term
  deriving (Eq)

instance Show Term  where
  show term = case term of
    Variable name         ->
      name
    Abstraction name term ->
      L.parenOpen ++ L.lambda ++ name ++ " " ++ L.arrow ++ " "
      ++ show term ++ L.parenClose
    Application t1 t2     ->
      show t1 ++ " " ++ show  t2
