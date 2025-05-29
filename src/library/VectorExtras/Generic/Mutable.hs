module VectorExtras.Generic.Mutable where

import Data.Vector.Generic.Mutable hiding (forM_)
import VectorExtras.Prelude

{-# INLINE writeListInReverseOrderStartingFrom #-}
writeListInReverseOrderStartingFrom :: (MVector v a) => v s a -> Int -> [a] -> ST s ()
writeListInReverseOrderStartingFrom v =
  let loop !index = \case
        value : tail -> do
          unsafeWrite v index value
          loop (pred index) tail
        _ -> return ()
   in loop

{-# INLINE writeAssocList #-}
writeAssocList :: (MVector v a) => v s a -> [(Int, a)] -> ST s ()
writeAssocList v =
  traverse_ $ \(i, value) -> unsafeWrite v i value
