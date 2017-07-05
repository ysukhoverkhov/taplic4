module Main where

import           Evaluation (eval)
import           Parser     (parse)

main :: IO ()
main =
  print $ parse "(~a -> a) (~b -> b)"
