module Main where

import           Evaluation (eval)
import           Parser     (parse)

example1 = "(~a -> ~b -> a b) (~b -> b) (~d -> d)"
example2 = "(~a -> ~b -> a (~a -> a b)) (~d -> d)"

main :: IO ()
main = do
  let parsed = parse example2
  print parsed
  let evaluated = parsed >>= eval
  print evaluated
