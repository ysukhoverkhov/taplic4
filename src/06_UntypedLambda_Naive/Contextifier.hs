module Contextifier (
  contextify
) where

import qualified Data.Map as Map
import           Syntax   (Context, Name (..), Term (..))

-- TODO invent a good name here.
newtype TermContexts = TermContexts (Map.Map String Context)

contextify :: Term -> Term
contextify = contextify' emptyContext

contextify' :: TermContexts -> Term -> Term
contextify' contexts term = case term of
  Variable (Name name _) -> Variable (Name name (Just $ valueForName contexts name))
  Application t1 t2      -> Application (contextify' contexts t1) (contextify' contexts t2)
  Abstraction (Name name _) term ->
    let
      newContexts = shiftContextsForName contexts name
      nameContext = valueForName newContexts name
    in Abstraction (Name name (Just nameContext)) (contextify' newContexts term)

valueForName :: TermContexts -> String -> Context
valueForName (TermContexts ctxts) name
  | Map.notMember name ctxts = 0
  | otherwise = ctxts Map.! name


-- TODO: stop assuming context is an integer.
shiftContextsForName :: TermContexts -> String -> TermContexts
shiftContextsForName (TermContexts contexts) name =
  TermContexts $ Map.insert name (valueForName (TermContexts contexts) name + 1) contexts

emptyContext :: TermContexts
emptyContext = TermContexts Map.empty
