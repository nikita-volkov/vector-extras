{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module VectorExtras.Immutable where

import Data.Vector.Generic
import VectorExtras.Prelude hiding (length)

-- |
-- Distribute the elements of a vector across the specified amount of chunks.
--
-- Depending on the size of the vector the first chunks may be larger in size then the others by one.
chunk :: (Vector v1 a, Vector v2 (v1 a)) => Int -> v1 a -> v2 (v1 a)
chunk chunksAmount vector =
  let vectorLength = length vector
      smallerChunkSize = div vectorLength chunksAmount
      largerChunkSize = succ smallerChunkSize
      largerChunksAmount = vectorLength - smallerChunkSize * chunksAmount
      largerChunksElementsAmount = largerChunksAmount * largerChunkSize
   in generate chunksAmount $ \chunkIndex ->
        let chunkOriginalIndex =
              if largerChunksAmount > chunkIndex
                then chunkIndex * largerChunkSize
                else largerChunksElementsAmount + (chunkIndex - largerChunksAmount) * smallerChunkSize
         in generate (if chunkIndex < largerChunksAmount then largerChunkSize else smallerChunkSize) $ \elemIndex ->
              unsafeIndex vector
                $ chunkOriginalIndex
                + elemIndex
