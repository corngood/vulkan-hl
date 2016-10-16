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

withInstance :: MonadIO m => InstanceCreateInfo -> InstanceM x m a -> m a
withInstance ci (InstanceM m) = do
  i <- liftIO $ Core.createInstance ci
  a <- runReaderT m i
  liftIO $ Core.destroyInstance i
  pure a

createDebugReportCallback :: MonadIO m => DebugReportFlags -> DebugReportCallbackFun -> InstanceM x m DebugReportCallback
createDebugReportCallback = liftR2 Core.createDebugReportCallback

createSurface :: MonadIO m => Window -> InstanceM x m (OwnedBy x Surface)
createSurface w = OwnedBy <$> liftR0 (Core.createSurface w)

createSwapchain :: MonadIO m => SwapchainCreateInfo -> DeviceM x m (OwnedBy x Swapchain)
createSwapchain ci = OwnedBy <$> liftR1 Core.createSwapchain ci

physicalDevices :: MonadIO m => InstanceM x m [OwnedBy x PhysicalDevice]
physicalDevices = fmap OwnedBy <$> liftR0 Core.physicalDevices

queueFamilyProperties :: MonadIO m => OwnedBy x PhysicalDevice -> InstanceM x m [QueueFamilyProperties]
queueFamilyProperties (OwnedBy pd) = liftIO (Core.queueFamilyProperties pd)

queueFamilySupportsPresent :: MonadIO m => OwnedBy x PhysicalDevice -> Word -> OwnedBy x Surface -> InstanceM x m Bool
queueFamilySupportsPresent (OwnedBy pd) qi (OwnedBy s) = liftIO (Core.queueFamilySupportsPresent pd qi s)

surfaceFormats :: MonadIO m => OwnedBy x PhysicalDevice -> OwnedBy x Surface -> InstanceM x m [SurfaceFormat]
surfaceFormats (OwnedBy pd) (OwnedBy s) = liftIO (Core.surfaceFormats pd s)

surfaceCapabilities :: MonadIO m => OwnedBy x PhysicalDevice -> OwnedBy x Surface -> InstanceM x m SurfaceCapabilities
surfaceCapabilities (OwnedBy pd) (OwnedBy s) = liftIO (Core.surfaceCapabilities pd s)

withDevice :: MonadIO m => OwnedBy y PhysicalDevice -> DeviceCreateInfo -> DeviceM x (InstanceM y m) a -> InstanceM y m a
withDevice (OwnedBy pd) ci (DeviceM m) = do
  i <- liftIO $ Core.createDevice pd ci
  a <- runReaderT m i
  -- liftIO $ Core.destroyDevice i
  pure a

getQueue :: MonadIO m => Word -> Word -> DeviceM x m Queue
getQueue = liftR2 Core.getQueue

createCommandPool :: MonadIO m => CommandPoolCreateInfo -> DeviceM x m CommandPool
createCommandPool = liftR1 Core.createCommandPool
