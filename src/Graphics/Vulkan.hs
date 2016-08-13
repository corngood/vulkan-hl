module Graphics.Vulkan
  ( InstanceM
  , withInstance
  , createDebugReportCallback
  , createSurface
  , module Graphics.Vulkan.Types
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import SDL.Internal.Types (Window)

import Graphics.Vulkan.Internal
import Graphics.Vulkan.Types
import qualified Graphics.Vulkan.Core as Core

withInstance :: MonadIO m => InstanceCreateInfo -> InstanceM m a -> m a
withInstance ci (InstanceM m) = do
  i <- liftIO $ Core.createInstance ci
  a <- runReaderT m i
  liftIO $ Core.destroyInstance i
  pure a

createDebugReportCallback :: MonadIO m => DebugReportFlags -> DebugReportCallbackFun -> InstanceM m DebugReportCallback
createDebugReportCallback = liftR2 Core.createDebugReportCallback

createSurface :: MonadIO m => Window -> InstanceM m Surface
createSurface = liftR1 $ flip Core.createSurface
