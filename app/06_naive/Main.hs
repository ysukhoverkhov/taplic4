module Main where

import           Evaluation (eval)
import           Parser     (parse)

main :: IO ()
main =
  print $ parse "(~a -> ~b -> a b) (~c -> c) (~d -> d)" >>= eval
