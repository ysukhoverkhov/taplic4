module Main where


data Term =
  TmTrue |
  TmFalse |
  TmIf Term Term Term |
  TmZero |
  TmSucc Term |
  TmPred Term |
  TmIsZero Term
  deriving (Show)


isNumericVal :: Term -> Bool
isNumericVal TmZero = True
isNumericVal (TmSucc t) = isNumericVal t
isNumericVal _ = False

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t
  | isNumericVal t = True
  | otherwise = False

eval :: Term -> Term
eval (TmIf TmTrue t2 _) = t2
eval (TmIf TmFalse _ t3) = t3
eval (TmIf t1 t2 t3) = TmIf (eval t1) t2 t3
eval (TmIsZero TmZero) = TmTrue
eval (TmIsZero t)
  | isNumericVal t = TmFalse
  | otherwise = TmIsZero $ eval t
eval (TmSucc t) = TmSucc $ eval t
eval (TmPred (TmSucc t)) = t -- TODO: is num.



evalAll :: Term -> Term
evalAll t
  | isVal t = t
  | otherwise = evalAll $ eval t

main :: IO ()
main = do
  -- let t0 = TmIf TmTrue TmFalse TmFalse
  -- let e1 = evalAll $ TmIsZero (TmIf t0 TmZero (TmIsZero TmZero))
  -- print e1
  let e = evalAll (TmIsZero (TmIf TmTrue (TmSucc TmZero) TmZero))
  print e
