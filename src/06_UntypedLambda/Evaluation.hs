module Evaluation where

import Syntax

isVal :: Context -> Term -> Bool
isVal ctx term = case term of
  (TmAbs _ _) -> True
  _ -> False
