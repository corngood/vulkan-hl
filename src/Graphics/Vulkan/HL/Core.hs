{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
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
     wrapValue (applicationVersion a) $
     wrapString (engineName a) $
     wrapValue (engineVersion a) $
     wrapValue (apiVersion a)
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

data Instance = Instance VkInstance
              deriving (Eq, Ord, Show)

instance FromVk Instance VkInstance where
  fromVk = return . Instance

data Surface = Surface VkSurfaceKHR
              deriving (Eq, Show)

instance FromVk Surface VkSurfaceKHR where
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

data PhysicalDevice = PhysicalDevice VkPhysicalDevice
                    deriving (Eq, Ord, Show)

instance FromVk PhysicalDevice VkPhysicalDevice where
  fromVk = return . PhysicalDevice

data QueueFamily = QueueFamily PhysicalDevice Int
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

createInstance :: InstanceCreateInfo -> IO Instance
createInstance a =
  (wrapInPtr a $
   wrapConst nullPtr $
   wrapOutPtr id
  ) vkCreateInstance

deviceExtensionProperties :: IO [Extension]
deviceExtensionProperties = wrapCountArray $ vkEnumerateInstanceExtensionProperties nullPtr

physicalDevices :: Instance -> IO [PhysicalDevice]
physicalDevices (Instance i) = wrapCountArray $ vkEnumeratePhysicalDevices i

queueFamilyProperties :: PhysicalDevice -> IO [QueueFamilyProperties]
queueFamilyProperties d@(PhysicalDevice h) =
  zipWith setFamily [0..] <$> wrapCountArray (vkGetPhysicalDeviceQueueFamilyProperties h)
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

data Device = Device VkDevice
            deriving (Eq, Ord, Show)

instance FromVk Device VkDevice where
  fromVk = return . Device

createDevice :: PhysicalDevice -> DeviceCreateInfo -> IO Device
createDevice (PhysicalDevice pd) a =
  wrapInPtr a
  (wrapConst nullPtr $
   wrapOutPtr id
  ) $ vkCreateDevice pd

data Queue = Queue VkQueue
           deriving (Eq, Ord, Show)

instance FromVk Queue VkQueue where
  fromVk = return . Queue

getQueue :: Device -> QueueFamily -> Int -> IO Queue
getQueue (Device d) (QueueFamily _ f) i =
  wrapOutPtr id
  $ vkGetDeviceQueue d (fromIntegral f) (fromIntegral i)

data CommandPoolCreateInfo = CommandPoolCreateInfo { comamndPoolQueueFamily :: QueueFamily
                                                   }

instance WithVk CommandPoolCreateInfo VkCommandPoolCreateInfo where
  withVk (CommandPoolCreateInfo (QueueFamily _ qfi)) f =
    f $ VkCommandPoolCreateInfo VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO nullPtr
    (VkCommandPoolCreateFlagBits zeroBits) (fromIntegral qfi)

data CommandPool = CommandPool VkCommandPool
                 deriving (Eq, Ord, Show)

instance FromVk CommandPool VkCommandPool where
  fromVk = return . CommandPool

createCommandPool :: Device -> CommandPoolCreateInfo -> IO CommandPool
createCommandPool (Device d) ci =
  wrapInPtr ci
  (wrapConst nullPtr $
   wrapOutPtr id)
  $ vkCreateCommandPool d
