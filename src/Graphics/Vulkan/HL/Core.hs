{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
{-# language TypeSynonymInstances #-}

module Graphics.Vulkan.HL.Core where

import Control.Monad
import Data.Bits
import Data.Vector.Storable.Sized
import Data.Word
import Foreign.C.String
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Graphics.Vulkan
import Graphics.Vulkan.HL.Internal.Marshal

import SDL.Internal.Types (Window(Window))
import SDL.Video.Vulkan

type LayerName = String
type ExtensionName = String
data Version = Version Int Int Int

instance WithVk Version Word32 where
  withVk (Version a b c) f = f v
    where v = vkMakeVersion (fromIntegral a) (fromIntegral b) (fromIntegral c)

data ApplicationInfo = ApplicationInfo { applicationName :: String
                                       , applicationVersion :: Version
                                       , engineName :: String
                                       , engineVersion :: Version
                                       , apiVersion :: Version
                                       }

instance WithVk ApplicationInfo VkApplicationInfo where
  withVk a f =
    (wrapString (applicationName a) $
     wrapValue (Graphics.Vulkan.HL.Core.applicationVersion a) $
     wrapString (engineName a) $
     wrapValue (Graphics.Vulkan.HL.Core.engineVersion a) $
     wrapValue (Graphics.Vulkan.HL.Core.apiVersion a)
     f)
    (VkApplicationInfo VK_STRUCTURE_TYPE_APPLICATION_INFO nullPtr)

data InstanceCreateInfo = InstanceCreateInfo { applicationInfo :: ApplicationInfo
                                             , instanceLayers :: [LayerName]
                                             , instanceExtensions :: [ExtensionName]
                                             }

instance WithVk InstanceCreateInfo VkInstanceCreateInfo where
  withVk (InstanceCreateInfo ai l e) f =
    (wrapInPtr ai $
     wrapArray l $
     wrapArray e
     f)
    (VkInstanceCreateInfo VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO nullPtr (VkInstanceCreateFlags zeroBits))

data Instance = Instance Graphics.Vulkan.Instance
              deriving (Eq, Ord, Show)

instance FromVk Graphics.Vulkan.HL.Core.Instance Graphics.Vulkan.Instance where
  fromVk = return . Instance

data Surface = Surface Graphics.Vulkan.SurfaceKHR
              deriving (Eq, Show)

instance FromVk Surface Graphics.Vulkan.SurfaceKHR where
  fromVk = return . Surface

data Extension = Extension { extensionName :: ExtensionName
                           , extensionVersion :: Int
                           }
               deriving (Eq, Ord, Show, Read)

instance FromVk Extension VkExtensionProperties where
  fromVk (VkExtensionProperties name version) = do
    let nameList = Prelude.reverse (Data.Vector.Storable.Sized.foldl' (flip (:)) [] name)
    withArray nameList (\pname -> do
                           n <- peekCString pname
                           return $ Extension n (fromIntegral version)
                       )

data PhysicalDevice = PhysicalDevice Graphics.Vulkan.PhysicalDevice
                    deriving (Eq, Ord, Show)

instance FromVk Graphics.Vulkan.HL.Core.PhysicalDevice Graphics.Vulkan.PhysicalDevice where
  fromVk = return . PhysicalDevice

data QueueFamily = QueueFamily Graphics.Vulkan.HL.Core.PhysicalDevice Int
                 deriving (Eq, Ord, Show)

data QueueFlags = QueueFlags { queueGraphics :: Bool
                             , queueCompute :: Bool
                             , queueTransfer :: Bool
                             , queueSparseBinding :: Bool
                             }
                deriving (Eq, Ord, Show, Read)

hasFlag :: Bits a => a -> a -> Bool
hasFlag a b = a .|. b /= zeroBits

instance FromVk QueueFlags VkQueueFlags where
  fromVk f =
    return $ QueueFlags
      (f `hasFlag` VK_QUEUE_GRAPHICS_BIT)
      (f `hasFlag` VK_QUEUE_COMPUTE_BIT)
      (f `hasFlag` VK_QUEUE_TRANSFER_BIT)
      (f `hasFlag` VK_QUEUE_SPARSE_BINDING_BIT)

data QueueFamilyProperties = QueueFamilyProperties { queueFamily :: QueueFamily
                                                   , queueFlags :: QueueFlags
                                                   , queueCount :: Int
                                                   }
                           deriving (Eq, Ord, Show)

instance FromVk QueueFamilyProperties VkQueueFamilyProperties where
  fromVk (VkQueueFamilyProperties qf qc _ _) = do
    flags <- fromVk qf
    return $ QueueFamilyProperties undefined flags (fromIntegral qc)

createInstance :: InstanceCreateInfo -> IO Graphics.Vulkan.HL.Core.Instance
createInstance a =
  (wrapInPtr a $
   wrapConst nullPtr $
   wrapOutPtr id
  ) vkCreateInstance

destroyInstance :: Graphics.Vulkan.HL.Core.Instance -> IO ()
destroyInstance (Instance i) = vkDestroyInstance i nullPtr

deviceExtensionProperties :: IO [Extension]
deviceExtensionProperties = wrapCountArray $ vkEnumerateInstanceExtensionProperties nullPtr

physicalDevices :: Graphics.Vulkan.HL.Core.Instance -> IO [Graphics.Vulkan.HL.Core.PhysicalDevice]
physicalDevices (Instance i) = wrapCountArray $ vkEnumeratePhysicalDevices i

queueFamilyProperties :: Graphics.Vulkan.HL.Core.PhysicalDevice -> IO [QueueFamilyProperties]
queueFamilyProperties d@(PhysicalDevice h) =
  zipWith setFamily [0..] <$> wrapCountArray (vkGetPhysicalDeviceQueueFamilyProperties h)
  where setFamily a b = b { queueFamily = QueueFamily d a } :: QueueFamilyProperties

createSurface :: Window -> Graphics.Vulkan.HL.Core.Instance -> IO Surface
createSurface (Window w) (Instance i) =
  alloca (\ps -> do
             r <- createSurfaceFFI w i ps
             unless r $ error "SDL_CreateVulkanSurface failed"
             Surface <$> peek ps)

queueFamilySupportsPresent :: QueueFamily -> Surface -> IO Bool
queueFamilySupportsPresent (QueueFamily (PhysicalDevice d) qi) (Surface s) =
  wrapOutPtr id $
  vkGetPhysicalDeviceSurfaceSupportKHR d (fromIntegral qi) s

data QueueCreateInfo = QueueCreateInfo { queueCreateFamily :: QueueFamily
                                       , queueCreateCount :: Int
                                       }

instance WithVk QueueCreateInfo VkDeviceQueueCreateInfo where
  withVk (QueueCreateInfo (QueueFamily _ i) c) f =
    f $ VkDeviceQueueCreateInfo VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO nullPtr
    (VkDeviceQueueCreateFlags zeroBits) (fromIntegral i) (fromIntegral c) nullPtr

data DeviceCreateInfo = DeviceCreateInfo { deviceQueues :: [QueueCreateInfo]
                                         , deviceLayers :: [LayerName]
                                         , deviceExtensions :: [ExtensionName]
                                         }

instance WithVk DeviceCreateInfo VkDeviceCreateInfo where
  withVk (DeviceCreateInfo q l e) f =
    (wrapArray q $
     wrapArray l $
     wrapArray e $
     f . ($ nullPtr))
    (VkDeviceCreateInfo VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO nullPtr (VkDeviceCreateFlags zeroBits))

data Device = Device Graphics.Vulkan.Device
            deriving (Eq, Ord, Show)

instance FromVk Graphics.Vulkan.HL.Core.Device Graphics.Vulkan.Device where
  fromVk = return . Device

createDevice :: Graphics.Vulkan.HL.Core.PhysicalDevice -> DeviceCreateInfo -> IO Graphics.Vulkan.HL.Core.Device
createDevice (PhysicalDevice pd) a =
  wrapInPtr a
  (wrapConst nullPtr $
   wrapOutPtr id
  ) $ vkCreateDevice pd

data Queue = Queue Graphics.Vulkan.Queue
           deriving (Eq, Ord, Show)

instance FromVk Graphics.Vulkan.HL.Core.Queue Graphics.Vulkan.Queue where
  fromVk = return . Queue

getQueue :: Graphics.Vulkan.HL.Core.Device -> QueueFamily -> Int -> IO Graphics.Vulkan.HL.Core.Queue
getQueue (Device d) (QueueFamily _ f) i =
  wrapOutPtr id
  $ vkGetDeviceQueue d (fromIntegral f) (fromIntegral i)

data CommandPoolCreateInfo = CommandPoolCreateInfo { comamndPoolQueueFamily :: QueueFamily
                                                   }

instance WithVk CommandPoolCreateInfo VkCommandPoolCreateInfo where
  withVk (CommandPoolCreateInfo (QueueFamily _ qfi)) f =
    f $ VkCommandPoolCreateInfo VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO nullPtr
    zeroBits (fromIntegral qfi)

data CommandPool = CommandPool Graphics.Vulkan.CommandPool
                 deriving (Eq, Ord, Show)

instance FromVk Graphics.Vulkan.HL.Core.CommandPool Graphics.Vulkan.CommandPool where
  fromVk = return . Graphics.Vulkan.HL.Core.CommandPool

createCommandPool :: Graphics.Vulkan.HL.Core.Device -> CommandPoolCreateInfo -> IO Graphics.Vulkan.HL.Core.CommandPool
createCommandPool (Device d) ci =
  wrapInPtr ci
  (wrapConst nullPtr $
   wrapOutPtr id)
  $ vkCreateCommandPool d

data SurfaceFormat = SurfaceFormat VkSurfaceFormatKHR
                   deriving (Eq, Ord, Show)

instance FromVk SurfaceFormat VkSurfaceFormatKHR where
  fromVk = return . SurfaceFormat

surfaceFormats :: Graphics.Vulkan.HL.Core.PhysicalDevice -> Surface -> IO [SurfaceFormat]
surfaceFormats (PhysicalDevice pd) (Surface s) =
  wrapCountArray $ vkGetPhysicalDeviceSurfaceFormatsKHR pd s
