module Contextifier (
  contextify
) where

import           Syntax (Name (..), Term (..))

contextify :: Term -> Term
contextify term = case term of
  Variable (Name name _) -> Variable (Name name (Just 1))
  Application t1 t2      -> Application (contextify t1) (contextify t2)
  Abstraction (Name name _) term -> Abstraction (Name name (Just 1)) (contextify term)
