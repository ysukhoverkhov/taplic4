module Syntax (
  Term(..),
  NameBind,
  Context,
  termSubstTop
) where

-- TODO: add errors here instead of Maybe

import Data.List (find, elemIndex)
import Data.Maybe (fromMaybe)

-- Data types
type Name = String

data Term =
  Variable Int Int |
  Abstraction Name Term |
  Application Term Term
  deriving (Show)

data NameBind = NameBind deriving Show
type Binding = NameBind

type ContextElement = (Name, Binding)
newtype Context = Context [ContextElement] deriving Show

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

pickFreshName :: Context -> Name -> (Context, Name)
pickFreshName ctx name =
  if isNameBound ctx name then
    pickFreshName ctx (name ++ "'")
  else
    (addName ctx name, name)

indexToName :: Context -> Int -> Maybe Name
indexToName ctx index =
  fst <$> contextElementByIndex ctx index

indexToBinding :: Context -> Int -> Maybe NameBind
indexToBinding ctx index =
  snd <$> contextElementByIndex ctx index

contextElementByIndex :: Context -> Int -> Maybe ContextElement
contextElementByIndex (Context ctx) index
  | index >= length ctx = Nothing
  | otherwise = Just $ ctx !! index

nameToIndex :: Context -> Name -> Maybe Int
nameToIndex (Context ctx) name =
  testIndex ctx name 0
  where
    testIndex :: [ContextElement] -> Name -> Int -> Maybe Int
    testIndex ctx name currentIndex
      | null ctx = Nothing
      | name == fst (head ctx) = Just currentIndex
      | otherwise = testIndex (tail ctx) name (currentIndex + 1)


-- Other. Somethign useful
-- TODO: it looks like it's not in its place
printTm :: Context -> Term -> String
printTm ctx term = case term of
  Variable index maxLength | ctxLength ctx == maxLength ->
                            show (indexToName ctx index) -- TODO: maybe
                        | otherwise ->
                            "[bad index]"
  Abstraction name t1 ->
    let (ctx', x') = pickFreshName ctx name in
      "(lambda " ++ x' ++ ". " ++ printTm ctx' t1 ++ ")"
  Application t1 t2 ->
    "(" ++ printTm ctx t1 ++ " " ++ printTm ctx t2 ++ ")"


-- Shifting

termShift :: Int -> Term -> Term
termShift d = walk 0
  where
    walk c t = case t of
      Variable x n | x >= c -> Variable (x + d) (n + d)
                | otherwise -> Variable x (n + d)
      Abstraction name t1 -> Abstraction name (walk (c + 1) t1)
      Application t1 t2 -> Application (walk c t1) (walk c t2)

-- Substitution
termSubstTop :: Term -> Term -> Term
termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

-- [j -> s]t
termSubst :: Int -> Term -> Term -> Term
termSubst j s = walk 0
  where
    walk c t = case t of
      Variable x n | x == j + c -> termShift c s
                | otherwise -> Variable x n
      Abstraction name t1 -> Abstraction name (walk (c + 1) t1)
      Application t1 t2 -> Application (walk c t1) (walk c t2)
