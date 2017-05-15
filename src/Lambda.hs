module Lambda where

-- TODO: specify terms to export.

data Term =
  TmVar Integer Integer |
  TmAbs Term String |
  TmApp Term Term
  deriving (Show)

newtype Context = Context [(String, Binding)]

type Binding = String

printTm :: Context -> Term -> IO()
printTm ctx t = print "I'll implement it a bit later. I Promise."

termShift :: Integer -> Term -> Term
termShift d = walk 0
  where
    walk c t = case t of
      TmVar x n | x >= c -> TmVar (x + d) (n + d)
                | otherwise -> TmVar x (n + d)
      TmAbs t1 x -> TmAbs (walk (c + 1) t1) x
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)

-- [j -> s]t
termSubst :: Integer -> Term -> Term -> Term
termSubst j s = walk 0
  where
    walk c t = case t of
      TmVar x n | x == j + c -> termShift c s
                | otherwise -> TmVar x n
      TmAbs t1 x -> TmAbs (walk (c + 1) t1) x
      TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
