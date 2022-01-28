module VectorExtras.Combinators.Helpers where

import VectorExtras.Prelude

orPure :: (Alternative m) => a -> m a -> m a
orPure res main =
  main <|> pure res
