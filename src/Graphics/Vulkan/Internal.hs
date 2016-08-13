{-# language GeneralizedNewtypeDeriving #-}

module Graphics.Vulkan.Internal where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Reader

import Graphics.Vulkan.Types (Instance)

liftR1 :: (MonadReader r m, MonadIO m) => (r -> b -> IO a) -> b -> m a
liftR1 f b = ask >>= \r -> liftIO $ f r b
liftR2 :: (MonadReader r m, MonadIO m) => (r -> b -> c -> IO a) -> b -> c -> m a
liftR2 f b c = ask >>= \r -> liftIO $ f r b c

newtype InstanceM m a = InstanceM (ReaderT Instance m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Instance)
