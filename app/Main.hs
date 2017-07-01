module Main where

import ArExp (Term(..), evalAll)
import Syntax
import SyntaxNaive

main :: IO ()
main = do
  let e = evalAll (TmIsZero (TmIf TmZero (TmSucc TmZero) TmZero))
  print e
