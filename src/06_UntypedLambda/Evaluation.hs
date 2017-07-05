module Evaluation (
  eval
) where

import Syntax

isVal :: Context -> Term -> Bool
isVal ctx term = case term of
  (Abstraction _ _) -> True
  _ -> False

eval :: Context -> Term -> Maybe Term
eval ctx term
  | isVal ctx term = Just term
  | otherwise = evalSmallStep ctx term >>= eval ctx

evalSmallStep :: Context -> Term -> Maybe Term
evalSmallStep ctx term = case term of
  Application (Abstraction _ t12) v2 | isVal ctx v2 ->
    Just $ termSubstTop v2 t12

  Application v1 t2 | isVal ctx v1 ->
    let t2' = evalSmallStep ctx t2 in
    Application v1 <$> t2'

  Application t1 t2 ->
    let t1' = evalSmallStep ctx t1 in
    (`Application` t2) <$> t1'

  _ -> Nothing
