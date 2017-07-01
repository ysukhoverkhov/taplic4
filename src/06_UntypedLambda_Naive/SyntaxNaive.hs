module SyntaxNaive (
  Term(..), Name
) where

-- Data types
type Name = String

data Term =
  TmVar Name |
  TmAbs Name Term |
  TmApp Term Term
  deriving (Show)
