module Evaluation (
  eval
) where

import           Syntax (Name, Term (..))

eval :: Term -> Either String Term
eval term
  | isVal term = Right term
  | otherwise = evalSmallStep term >>= eval

isVal :: Term -> Bool
isVal term = case term of
  (Abstraction _ _) -> True
  _                 -> False

evalSmallStep :: Term -> Either String Term
evalSmallStep term = case term of
  Application (Abstraction name t12) v2 | isVal v2 ->
    Right $ termSubst name v2 t12

  Application v1 t2 | isVal v1 ->
    let t2' = evalSmallStep t2 in
    Application v1 <$> t2'

  Application t1 t2 ->
    let t1' = evalSmallStep t1 in
    (`Application` t2) <$> t1'

  _ -> Left "Invalid term"

-- [j -> s]t
termSubst :: Name -> Term -> Term -> Term
termSubst j s t =
  case t of
    Variable name | name == j -> s
               | otherwise -> Variable name
    Abstraction name t1 -> Abstraction name (termSubst j s t1)
    Application t1 t2 -> Application (termSubst j s t1) (termSubst j s t2)
