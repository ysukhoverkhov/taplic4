module ArExp( Term(..), evalAll) where

data Term =
  TmTrue |
  TmFalse |
  TmIf Term Term Term |
  TmZero |
  TmSucc Term |
  TmPred Term |
  TmIsZero Term
  deriving (Show)

evalAll :: Term -> Either String Term
evalAll t
  | isVal t = Right t
  | otherwise = eval t >>= evalAll

eval :: Term -> Either String Term
eval t = case t of
  (TmIf TmTrue t2 _) -> Right t2
  (TmIf TmFalse _ t3) -> Right t3
  (TmIf t1 t2 t3) | isNumericVal t1 -> Left "Condition in TmIf can't be numeric"
                  | otherwise -> (\x -> TmIf x t2 t3) <$> eval t1

  (TmIsZero TmZero) -> Right TmTrue
  (TmIsZero t1) | isNumericVal t1 -> Right TmFalse
                | isVal t1 -> Left "Argument of TmIsZero should be numeric"
                | otherwise -> TmIsZero <$> eval t1

  (TmSucc t) -> TmSucc <$> eval t

  (TmPred TmZero) -> Right TmZero
  (TmPred (TmSucc t1)) -> Right t1
  (TmPred t1) -> TmPred <$> eval t1

  _ -> Left $ "Can't evaluate " ++ show t

isNumericVal :: Term -> Bool
isNumericVal t = case t of
  TmZero -> True
  (TmSucc t1) -> isNumericVal t1
  _ -> False

isVal :: Term -> Bool
isVal t = case t of
  TmTrue -> True
  TmFalse -> True
  t1 | isNumericVal t1 -> True
     | otherwise -> False
