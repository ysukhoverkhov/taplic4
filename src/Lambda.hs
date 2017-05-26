module Lambda where

-- TODO: specify terms to export.

import Data.List (find, elemIndex)
import Data.Maybe (fromMaybe)

-- Data types

data Term =
  TmVar Int Int |
  TmAbs Term String |
  TmApp Term Term
  deriving (Show)

data NameBind = NameBind
type Binding = NameBind

type Name = String

newtype Context = Context [(Name, Binding)]

-- Context management

emptyContext :: Context
emptyContext = Context []

ctxLength :: Context -> Int
ctxLength (Context ctx) = length ctx

addBinding :: Context -> Name -> Binding -> Context
addBinding (Context ctx) name bind = Context $ (name, bind) : ctx

addName :: Context -> Name -> Context
addName ctx name = addBinding ctx name NameBind

isNameBound :: Context -> Name -> Bool
isNameBound (Context ctx) name =
  any (\element -> fst element == name) ctx

-- TODO: replace string with Name

pickFreshName :: Context -> Name -> (Context, Name)
pickFreshName ctx name =
  if isNameBound ctx name then
    pickFreshName ctx (name ++ "'")
  else
    (addName ctx name, name)

index2name :: Context -> Int -> Maybe Name
index2name (Context ctx) index =
  fst <$> maybeElement
  where
    maybeElement :: Maybe (String, Binding)
    maybeElement
      | index >= length ctx = Nothing
      | otherwise = Just $ ctx !! index

name2index :: Context -> Name -> Maybe Int
name2index (Context ctx) name =
  testIndex ctx name 0
  where
    testIndex :: [(Name, Binding)] -> Name -> Int -> Maybe Int
    testIndex ctx name currentIndex
      | null ctx = Nothing
      | name == fst (head ctx) = Just currentIndex
      | otherwise = testIndex (tail ctx) name (currentIndex + 1)


-- printTm :: Context -> Term -> String
-- printTm ctx t = case t of
--   TmVar x n | ctxLength ctx == n -> show (index2name ctx x)
--             | otherwise -> "[bad index]"
--   TmAbs t1 x ->
--     let (ctx', x') = pickFreshName ctx x in
--       "(lambda " ++ x' ++ ". " ++ printTm ctx' t1 ++ ")"
--   TmApp t1 t2 ->
--     "(" ++ printTm ctx t1 ++ " " ++ printTm ctx t2 ++ ")"
--   where


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
