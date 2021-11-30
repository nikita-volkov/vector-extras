module VectorExtras.Accumulator
  ( toVector,
    Accumulator,
    add,
  )
where

import Data.Vector.Generic
import qualified VectorExtras.Generic as GenericExtras
import VectorExtras.Prelude hiding (length)

{-# INLINE toVector #-}
toVector :: Vector v a => Accumulator a -> v a
toVector (Accumulator size list) =
  GenericExtras.fromReverseListN size list

-- |
-- Accumulator for construction of vectors.
--
-- Useful as an accumulator for in folding.
data Accumulator a
  = Accumulator !Int ![a]

{-# INLINE add #-}
add :: a -> Accumulator a -> Accumulator a
add head (Accumulator size tail) =
  Accumulator (succ size) (head : tail)
