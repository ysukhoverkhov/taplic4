module Lambda where

-- TODO: specify terms to export.

import Data.List (find)
import Data.Maybe (fromMaybe)

data Term =
  TmVar Int Int |
  TmAbs Term String |
  TmApp Term Term
  deriving (Show)

newtype Context = Context [(String, Binding)]

type Binding = Int

printTm :: Context -> Term -> String
printTm ctx t = case t of
  TmVar x n | ctxLength ctx == n -> show (index2name ctx x)
            | otherwise -> "[bad index]"
  TmAbs t1 x ->
    let (ctx', x') = pickFreshName ctx x in
      "(lambda " ++ x' ++ ". " ++ printTm ctx' t1 ++ ")"
  TmApp t1 t2 ->
    "(" ++ printTm ctx t1 ++ " " ++ printTm ctx t2 ++ ")"
  where
    pickFreshName ctx x = (ctx, x) -- TODO: implement me.
    ctxLength (Context ctx) = length ctx
    index2name :: Context -> Int -> String
    index2name (Context ctx) x =
      fromMaybe "[index not in context]" (fst <$> maybeElement)
      where
        maybeElement = find (\contextElement -> snd contextElement == x) ctx


termShift :: Int -> Term -> Term
termShift d = walk 0
  where
    walk c t = case t of
      TmVar x n | x >= c -> TmVar (x + d) (n + d)
                | otherwise -> TmVar x (n + d)
      TmAbs t1 x -> TmAbs (walk (c + 1) t1) x
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)

-- [j -> s]t
termSubst :: Int -> Term -> Term -> Term
termSubst j s = walk 0
  where
    walk c t = case t of
      TmVar x n | x == j + c -> termShift c s
                | otherwise -> TmVar x n
      TmAbs t1 x -> TmAbs (walk (c + 1) t1) x
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
