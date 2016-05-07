{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
{-# language TypeSynonymInstances #-}

module Graphics.Vulkan.HL.Core where

import Control.Monad
import Data.Bits
import Data.Vector.Storable.Sized hiding (head)
import Data.Word
import Foreign.C.String
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import qualified Graphics.Vulkan as Vk
import qualified Graphics.Vulkan.KHR.Surface as VkSu
import Graphics.Vulkan.HL.Internal.Marshal

import SDL.Internal.Types (Window(Window))
import SDL.Video.Vulkan

type LayerName = String
type ExtensionName = String
data Version = Version Int Int Int

instance WithVk Version Word32 where
  withVk (Version a b c) f = f v
    where v = Vk.makeVersion (fromIntegral a) (fromIntegral b) (fromIntegral c)

data ApplicationInfo = ApplicationInfo { applicationName :: String
                                       , applicationVersion :: Version
                                       , engineName :: String
                                       , engineVersion :: Version
                                       , apiVersion :: Version
                                       }

instance WithVk ApplicationInfo Vk.ApplicationInfo where
  withVk a f =
    (wrapString (applicationName a) $
     wrapValue (applicationVersion a) $
     wrapString (engineName a) $
     wrapValue (engineVersion a) $
     wrapValue (apiVersion a)
     f)
    (Vk.ApplicationInfo Vk.StructureTypeApplicationInfo nullPtr)

data InstanceCreateInfo = InstanceCreateInfo { applicationInfo :: ApplicationInfo
                                             , instanceLayers :: [LayerName]
                                             , instanceExtensions :: [ExtensionName]
                                             }

instance WithVk InstanceCreateInfo Vk.InstanceCreateInfo where
  withVk (InstanceCreateInfo ai l e) f =
    (wrapInPtr ai $
     wrapInArray l $
     wrapInArray e
     f)
    (Vk.InstanceCreateInfo Vk.StructureTypeInstanceCreateInfo nullPtr (Vk.InstanceCreateFlags zeroBits))

data Instance = Instance Vk.Instance
              deriving (Eq, Ord, Show)

instance FromVk Instance Vk.Instance where
  fromVk = return . Instance

data Surface = Surface Vk.Surface
              deriving (Eq, Show)

instance WithVk Surface Vk.Surface where
  withVk (Surface a) f = f a

instance FromVk Surface Vk.Surface where
  fromVk = return . Surface

data Extension = Extension { extensionName :: ExtensionName
                           , extensionVersion :: Int
                           }
               deriving (Eq, Ord, Show, Read)

instance FromVk Extension Vk.ExtensionProperties where
  fromVk (Vk.ExtensionProperties name version) = do
    let nameList = Prelude.reverse (Data.Vector.Storable.Sized.foldl' (flip (:)) [] name)
    withArray nameList (\pname -> do
                           n <- peekCString pname
                           return $ Extension n (fromIntegral version)
                       )

data PhysicalDevice = PhysicalDevice Vk.PhysicalDevice
                    deriving (Eq, Ord, Show)

instance FromVk PhysicalDevice Vk.PhysicalDevice where
  fromVk = return . PhysicalDevice

data QueueFamily = QueueFamily PhysicalDevice Int
                 deriving (Eq, Ord, Show)

instance WithVk QueueFamily Word32 where
  withVk (QueueFamily _ i) f = f (fromIntegral i)

data QueueFlags = QueueFlags { queueGraphics :: Bool
                             , queueCompute :: Bool
                             , queueTransfer :: Bool
                             , queueSparseBinding :: Bool
                             }
                deriving (Eq, Ord, Show, Read)

hasFlag :: Bits a => a -> a -> Bool
hasFlag a b = a .|. b /= zeroBits

instance FromVk QueueFlags Vk.QueueFlags where
  fromVk f =
    return $ QueueFlags
      (f `hasFlag` Vk.QueueGraphicsBit)
      (f `hasFlag` Vk.QueueComputeBit)
      (f `hasFlag` Vk.QueueTransferBit)
      (f `hasFlag` Vk.QueueSparseBindingBit)

data QueueFamilyProperties = QueueFamilyProperties { queueFamily :: QueueFamily
                                                   , queueFlags :: QueueFlags
                                                   , queueCount :: Int
                                                   }
                           deriving (Eq, Ord, Show)

instance FromVk QueueFamilyProperties Vk.QueueFamilyProperties where
  fromVk (Vk.QueueFamilyProperties qf qc _ _) = do
    flags <- fromVk qf
    return $ QueueFamilyProperties undefined flags (fromIntegral qc)

createInstance :: InstanceCreateInfo -> IO Instance
createInstance a =
  (wrapInPtr a $
   wrapConst nullPtr $
   wrapOutPtr id
  ) Vk.createInstance

destroyInstance :: Instance -> IO ()
destroyInstance (Instance i) = Vk.destroyInstance i nullPtr

deviceExtensionProperties :: IO [Extension]
deviceExtensionProperties = wrapCountArray $ Vk.enumerateInstanceExtensionProperties nullPtr

physicalDevices :: Instance -> IO [PhysicalDevice]
physicalDevices (Instance i) = wrapCountArray $ Vk.enumeratePhysicalDevices i

queueFamilyProperties :: PhysicalDevice -> IO [QueueFamilyProperties]
queueFamilyProperties d@(PhysicalDevice h) =
  zipWith setFamily [0..] <$> wrapCountArray (Vk.getPhysicalDeviceQueueFamilyProperties h)
  where setFamily a b = b { queueFamily = QueueFamily d a } :: QueueFamilyProperties

createSurface :: Window -> Instance -> IO Surface
createSurface (Window w) (Instance i) =
  alloca (\ps -> do
             r <- createSurfaceFFI w i ps
             unless r $ error "SDL_CreateVulkanSurface failed"
             Surface <$> peek ps)

queueFamilySupportsPresent :: QueueFamily -> Surface -> IO Bool
queueFamilySupportsPresent (QueueFamily (PhysicalDevice d) qi) (Surface s) =
  wrapOutPtr id $
  Vk.getPhysicalDeviceSurfaceSupport d (fromIntegral qi) s

data QueueCreateInfo = QueueCreateInfo { queueCreateFamily :: QueueFamily
                                       , queueCreateCount :: Int
                                       }

instance WithVk QueueCreateInfo Vk.DeviceQueueCreateInfo where
  withVk (QueueCreateInfo (QueueFamily _ i) c) f =
    f $ Vk.DeviceQueueCreateInfo Vk.StructureTypeDeviceQueueCreateInfo nullPtr
    (Vk.DeviceQueueCreateFlags zeroBits) (fromIntegral i) (fromIntegral c) nullPtr

data DeviceCreateInfo = DeviceCreateInfo { deviceQueues :: [QueueCreateInfo]
                                         , deviceLayers :: [LayerName]
                                         , deviceExtensions :: [ExtensionName]
                                         }

instance WithVk DeviceCreateInfo Vk.DeviceCreateInfo where
  withVk (DeviceCreateInfo q l e) f =
    (wrapInArray q $
     wrapInArray l $
     wrapInArray e $
     f . ($ nullPtr))
    (Vk.DeviceCreateInfo Vk.StructureTypeDeviceCreateInfo nullPtr (Vk.DeviceCreateFlags zeroBits))

data Device = Device Vk.Device
            deriving (Eq, Ord, Show)

instance FromVk Device Vk.Device where
  fromVk = return . Device

createDevice :: PhysicalDevice -> DeviceCreateInfo -> IO Device
createDevice (PhysicalDevice pd) a =
  wrapInPtr a
  (wrapConst nullPtr $
   wrapOutPtr id
  ) $ Vk.createDevice pd

data Queue = Queue Vk.Queue
           deriving (Eq, Ord, Show)

instance FromVk Queue Vk.Queue where
  fromVk = return . Queue

getQueue :: Device -> QueueFamily -> Int -> IO Queue
getQueue (Device d) (QueueFamily _ f) i =
  wrapOutPtr id
  $ Vk.getDeviceQueue d (fromIntegral f) (fromIntegral i)

data CommandPoolCreateInfo = CommandPoolCreateInfo { comamndPoolQueueFamily :: QueueFamily
                                                   }

instance WithVk CommandPoolCreateInfo Vk.CommandPoolCreateInfo where
  withVk (CommandPoolCreateInfo (QueueFamily _ qfi)) f =
    f $ Vk.CommandPoolCreateInfo Vk.StructureTypeCommandPoolCreateInfo nullPtr
    zeroBits (fromIntegral qfi)

data CommandPool = CommandPool Vk.CommandPool
                 deriving (Eq, Ord, Show)

instance FromVk CommandPool Vk.CommandPool where
  fromVk = return . CommandPool

createCommandPool :: Device -> CommandPoolCreateInfo -> IO CommandPool
createCommandPool (Device d) ci =
  wrapInPtr ci
  (wrapConst nullPtr $
   wrapOutPtr id)
  $ Vk.createCommandPool d

surfaceFormats :: PhysicalDevice -> Surface -> IO [Vk.SurfaceFormat]
surfaceFormats (PhysicalDevice pd) (Surface s) =
  wrapCountArray $ Vk.getPhysicalDeviceSurfaceFormats pd s

surfaceCapabilities :: PhysicalDevice -> Surface -> IO Vk.SurfaceCapabilities
surfaceCapabilities (PhysicalDevice pd) (Surface s) =
  wrapOutPtr id $ Vk.getPhysicalDeviceSurfaceCapabilities pd s

data CommandBuffer = CommandBuffer Vk.CommandBuffer
                 deriving (Eq, Ord, Show)

instance FromVk CommandBuffer Vk.CommandBuffer where
  fromVk = return . CommandBuffer

allocateCommandBuffers :: Device -> CommandPool -> Vk.CommandBufferLevel -> Int -> IO [CommandBuffer]
allocateCommandBuffers (Device d) (CommandPool cp) l n =
  wrapInPtr (Vk.CommandBufferAllocateInfo Vk.StructureTypeCommandBufferAllocateInfo nullPtr cp l (fromIntegral n))
  (wrapOutArray n id)
  $ Vk.allocateCommandBuffers d

allocateCommandBuffer :: Device -> CommandPool -> Vk.CommandBufferLevel -> IO CommandBuffer
allocateCommandBuffer d cp l = head <$> allocateCommandBuffers d cp l 1

data SwapchainCreateInfo = SwapchainCreateInfo { flags :: Vk.SwapchainCreateFlags
                                               , surface :: Surface
                                               , minImageCount :: Int
                                               , imageFormat :: Vk.SurfaceFormat
                                               , imageExtent :: Vk.Extent2D
                                               , imageArrayLayers :: Int
                                               , imageUsage :: Vk.ImageUsageFlags
                                               , imageSharingMode :: Vk.SharingMode
                                               , queueFamilyIndices :: [QueueFamily]
                                               , preTransform :: Vk.SurfaceTransformFlags
                                               , compositeAlpha :: Vk.CompositeAlphaFlags
                                               , presentMode :: Vk.PresentMode
                                               , clipped :: Bool
                                               }

instance WithVk SwapchainCreateInfo Vk.SwapchainCreateInfo where
  withVk s f =
    (wrapConst (flags s) $
     wrapValue (surface s) $
     wrapValue (minImageCount s) $
     wrapConst (VkSu.format $ imageFormat s) $
     wrapConst (Vk.colorSpace $ imageFormat s) $
     wrapConst (imageExtent s) $
     wrapValue (imageArrayLayers s) $
     wrapConst (imageUsage s) $
     wrapConst (imageSharingMode s) $
     wrapInArray (queueFamilyIndices s) $
     wrapConst (preTransform s) $
     wrapConst (compositeAlpha s) $
     wrapConst (presentMode s) $
     wrapValue (clipped s) $
     wrapConst (Vk.Swapchain 0)
     f)
    (Vk.SwapchainCreateInfo (Vk.StructureType 1000001000) nullPtr)

data Swapchain = Swapchain Vk.Swapchain
               deriving (Eq, Ord, Show)

instance FromVk Swapchain Vk.Swapchain where
  fromVk = return . Swapchain

createSwapchain :: Device -> SwapchainCreateInfo -> IO Swapchain
createSwapchain (Device d) ci =
  wrapInPtr ci
  (wrapConst nullPtr $
   wrapOutPtr id)
  $ Vk.createSwapchain d
