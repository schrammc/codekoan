module Thesis.Util.MonadUtils where

import Data.Maybe (catMaybes)
import Control.Monad.Trans.Maybe

catMaybeTs :: (Functor f, Monad f) => [MaybeT f a] -> MaybeT f [a]
catMaybeTs xs = MaybeT $ Just <$> maybeList
  where
    maybeList = catMaybes <$> (mapM runMaybeT xs)

  
