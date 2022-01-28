module VectorExtras.Combinators.Helpers where

import VectorExtras.Accumulator (Accumulator)
import qualified VectorExtras.Accumulator as Acc
import VectorExtras.Prelude

-- * General

orPure :: (Alternative m) => a -> m a -> m a
orPure res main =
  main <|> pure res

-- * Accumulator-oriented

accMany :: (MonadPlus m) => Accumulator a -> m a -> m (Accumulator a)
accMany acc getElement =
  collect acc
  where
    collect !acc =
      orPure acc $ getElement >>= collect . flip Acc.add acc
