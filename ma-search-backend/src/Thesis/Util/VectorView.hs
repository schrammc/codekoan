{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Thesis.Util.VectorView where

import qualified Data.Vector as V
import Data.Foldable

data VectorView a where
  VectorView :: (b -> a) -> (V.Vector b)  -> VectorView a

instance Eq a => Eq (VectorView a) where
  (VectorView mapperA vA) == (VectorView mapperB vB) =
    if V.length vA /= V.length vB
    then False
    else cmp 0
    where
      cmp i | i >= V.length vA = True
            | otherwise =
              if (mapperA $ V.unsafeIndex vA i) == (mapperB $ V.unsafeIndex vB i)
              then cmp (i+1)
              else False

instance Show a => Show (VectorView a) where
  show x = show $ toList x

(<$$$>) = VectorView

instance Functor VectorView where
  fmap f (VectorView mapper v) = VectorView (f . mapper) v

instance Foldable VectorView where
  foldr = foldrVectorView
  length (VectorView _ v) = V.length v

foldrVectorView :: (a -> b -> b) -> b -> VectorView a -> b
foldrVectorView f x (VectorView mapper v) = go (l-1) x
  where
    l = V.length v
    go i x | i < 0    = x
           | otherwise =
             let next = mapper ((V.unsafeIndex) v i)
             in go (i-1) $! f next x

toVector :: VectorView a -> V.Vector a
toVector (VectorView mapper v) = mapper <$> v

fromVector :: V.Vector a -> VectorView a
fromVector v = VectorView id v

unsafeSliceView :: Int -> Int -> VectorView a -> VectorView a
unsafeSliceView a b (VectorView mapper v) =
  VectorView mapper $ V.unsafeSlice a b v
