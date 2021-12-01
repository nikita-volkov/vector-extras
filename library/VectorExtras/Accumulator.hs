-- |
-- Utility for construction of vectors when the size is not known ahead.
module VectorExtras.Accumulator
  ( Accumulator,
    add,

    -- * Execution
    toVector,
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
-- Constructor of vectors optimised for appending elements one by one,
-- providing for \(\mathcal{O}(n)\) complexity of the whole construction process.
--
-- Very useful as accumulator in folds.
--
-- Under the hood it is the size counter and a reverse list of elements.
-- When executed, a vector of the according size gets allocated and
-- gets populated with the elements in reverse order
-- (starting from the last element).
data Accumulator a
  = Accumulator !Int ![a]

-- |
-- Add an element to the accumulator.
{-# INLINE add #-}
add :: a -> Accumulator a -> Accumulator a
add head (Accumulator size tail) =
  Accumulator (succ size) (head : tail)
