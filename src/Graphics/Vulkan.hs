module Graphics.Vulkan
  ( InstanceM
  , DeviceM
  , OwnedBy
  , withInstance
  , createDebugReportCallback
  , createSurface
  , createSwapchain
  , physicalDevices
  , queueFamilyProperties
  , queueFamilySupportsPresent
  , surfaceFormats
  , surfaceCapabilities
  , withDevice
  , getQueue
  , createCommandPool
  , module Graphics.Vulkan.Types
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import SDL.Internal.Types (Window)

import Graphics.Vulkan.Internal
import Graphics.Vulkan.Types
import qualified Graphics.Vulkan.Core as Core

withInstance :: MonadIO m => InstanceCreateInfo -> InstanceM i m a -> m a
withInstance ci (InstanceM m) = do
  i <- liftIO $ Core.createInstance ci
  a <- runReaderT m i
  liftIO $ Core.destroyInstance i
  pure a

createDebugReportCallback :: MonadIO m => DebugReportFlags -> DebugReportCallbackFun -> InstanceM i m DebugReportCallback
createDebugReportCallback = liftR2 Core.createDebugReportCallback

createSurface :: HasInstance m => Window -> m (Surface i)
createSurface w = askInstance >>= \r -> liftIO $ Core.createSurface w r

createSwapchain :: MonadIO m => SwapchainCreateInfo i -> DeviceM d (InstanceM i m) (Swapchain d)
createSwapchain = liftR1 Core.createSwapchain

physicalDevices :: MonadIO m => InstanceM i m [PhysicalDevice i]
physicalDevices = liftR0 Core.physicalDevices

queueFamilyProperties :: MonadIO m => PhysicalDevice i -> InstanceM i m [QueueFamilyProperties]
queueFamilyProperties pd = liftIO (Core.queueFamilyProperties pd)

queueFamilySupportsPresent :: MonadIO m => PhysicalDevice i -> Word -> Surface i -> InstanceM i m Bool
queueFamilySupportsPresent pd qi s = liftIO (Core.queueFamilySupportsPresent pd qi s)

surfaceFormats :: MonadIO m => PhysicalDevice i -> Surface i -> InstanceM i m [SurfaceFormat]
surfaceFormats pd s = liftIO (Core.surfaceFormats pd s)

surfaceCapabilities :: MonadIO m => PhysicalDevice i -> Surface i -> InstanceM i m SurfaceCapabilities
surfaceCapabilities pd s = liftIO (Core.surfaceCapabilities pd s)

withDevice :: MonadIO m => PhysicalDevice i -> DeviceCreateInfo -> DeviceM d (InstanceM i m) a -> InstanceM i m a
withDevice pd ci (DeviceM m) = do
  i <- liftIO $ Core.createDevice pd ci
  a <- runReaderT m i
  -- liftIO $ Core.destroyDevice i
  pure a

getQueue :: MonadIO m => Word -> Word -> DeviceM d m Queue
getQueue = liftR2 Core.getQueue

createCommandPool :: MonadIO m => CommandPoolCreateInfo -> DeviceM d m CommandPool
createCommandPool = liftR1 Core.createCommandPool
