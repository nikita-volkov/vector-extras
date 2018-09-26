module VectorExtras.Immutable where

import VectorExtras.Prelude hiding (length)
import Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable as Mutable
import qualified DeferredFolds.Unfoldr as Unfoldr
import qualified Data.HashMap.Strict as HashMap


{-|
Distribute the elements of a vector across the specified amount of chunks.

Depending on the size of the vector the first chunks may be larger in size then the others by one.
-}
chunk :: (Vector v1 a, Vector v2 (v1 a)) => Int -> v1 a -> v2 (v1 a)
chunk chunksAmount vector = let
  vectorLength = length vector
  smallerChunkSize = div vectorLength chunksAmount
  largerChunkSize = succ smallerChunkSize
  largerChunksAmount = vectorLength - smallerChunkSize * chunksAmount
  largerChunksElementsAmount = largerChunksAmount * largerChunkSize
  in
    generate chunksAmount $ \ chunkIndex -> let
      chunkOriginalIndex = if largerChunksAmount > chunkIndex
        then chunkIndex * largerChunkSize
        else largerChunksElementsAmount + (chunkIndex - largerChunksAmount) * smallerChunkSize
      in
        generate (if chunkIndex < largerChunksAmount then largerChunkSize else smallerChunkSize) $ \ elemIndex ->
        unsafeIndex vector $
        chunkOriginalIndex + elemIndex

{-|
Construct from an unfoldr of the specified size.

This function is partial. It doesn't check the size or indices.
-}
{-# INLINE unfoldrWithSize #-}
unfoldrWithSize :: (Mutable.MVector (Mutable vector) a, Vector vector a) => Int -> Unfoldr (Int, a) -> vector a
unfoldrWithSize size unfoldr =
  runST $ do
    mv <- Mutable.new size
    VectorExtras.Prelude.forM_ unfoldr $ \ (index, element) -> Mutable.write mv index element
    freeze mv

{-|
Construct from a hash-map of the specified size.

This function is partial. It doesn't check the size or indices.
-}
{-# INLINE indexHashMapWithSize #-}
indexHashMapWithSize :: (Mutable.MVector (Mutable vector) a, Vector vector a) => Int -> HashMap a Int -> vector a
indexHashMapWithSize size = unfoldrWithSize size . fmap swap . Unfoldr.hashMapAssocs

{-|
Construct from a hash-map.
-}
{-# INLINE indexHashMap #-}
indexHashMap :: (Mutable.MVector (Mutable vector) a, Vector vector a) => HashMap a Int -> vector a
indexHashMap hashMap = indexHashMapWithSize (HashMap.size hashMap) hashMap
