module EvaluationNaive (
  eval
) where

import           SyntaxNaive (Name, Term (..))

-- TODO: replace Maybe with Either

eval :: Term -> Maybe Term
eval term
  | isVal term = Just term
  | otherwise = evalSmallStep term >>= eval

isVal :: Term -> Bool
isVal term = case term of
  (TmAbs _ _) -> True
  _           -> False

evalSmallStep :: Term -> Maybe Term
evalSmallStep term = case term of
  TmApp (TmAbs name t12) v2 | isVal v2 ->
    Just $ termSubst name v2 t12

  TmApp v1 t2 | isVal v1 ->
    let t2' = evalSmallStep t2 in
    TmApp v1 <$> t2'

  TmApp t1 t2 ->
    let t1' = evalSmallStep t1 in
    (`TmApp` t2) <$> t1'

  _ -> Nothing

-- Other. Somethign useful
-- TODO: it looks like it's not in its place
-- TODO: replace with Show instance
printTm :: Term -> String
printTm term = case term of
  TmVar name      -> name
  TmAbs name term -> "(~" ++ name ++ " -> " ++ printTm term ++ ")"
  TmApp t1 t2     -> printTm t1 ++ " " ++ printTm  t2

-- [j -> s]t
termSubst :: Name -> Term -> Term -> Term
termSubst j s t =
  case t of
    TmVar name | name == j -> s
               | otherwise -> TmVar name
    TmAbs name t1 -> TmAbs name (termSubst j s t1)
    TmApp t1 t2 -> TmApp (termSubst j s t1) (termSubst j s t2)
