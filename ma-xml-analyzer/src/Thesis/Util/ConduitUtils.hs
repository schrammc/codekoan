-- | A module containing a number of helper functions to perform conduit related
-- tasks.
{-# LANGUAGE ScopedTypeVariables #-}
module Thesis.Util.ConduitUtils where

import Data.Conduit

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad

-- | This conduit will pass through a maximum of 'n' input values before
-- stopping processing.
maxElements :: (MonadIO m) => Int -> Conduit a m a
maxElements n = do
  counterVar <- liftIO $ newMVar 0
  go counterVar
  where
    go counterVar = do
      counter <- liftIO $ takeMVar counterVar
      if counter < n
        then do
          liftIO $ putMVar counterVar (counter + 1)
          x <- await
          forM_ x yield
          go counterVar

        else return ()
