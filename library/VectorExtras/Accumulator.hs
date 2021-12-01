module VectorExtras.Accumulator
  ( toVector,
    Accumulator,
    add,
  )
where

import Data.Vector.Generic
import qualified VectorExtras.Generic as GenericExtras
import VectorExtras.Prelude hiding (length)

-- |
-- Finalise the accumulator as vector.
{-# INLINE toVector #-}
toVector :: Vector v a => Accumulator a -> v a
toVector (Accumulator size list) =
  GenericExtras.fromReverseListN size list

-- |
-- Constructor of vectors optimised for appending elements one by one.
--
-- Very useful as accumulator in folds.
data Accumulator a
  = Accumulator !Int ![a]

-- |
-- Add an element to the accumulator.
{-# INLINE add #-}
add :: a -> Accumulator a -> Accumulator a
add head (Accumulator size tail) =
  Accumulator (succ size) (head : tail)
