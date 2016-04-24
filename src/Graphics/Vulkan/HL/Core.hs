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
    where v = Graphics.Vulkan.makeVersion (fromIntegral a) (fromIntegral b) (fromIntegral c)

data ApplicationInfo = ApplicationInfo { applicationName :: String
                                       , applicationVersion :: Version
                                       , engineName :: String
                                       , engineVersion :: Version
                                       , apiVersion :: Version
                                       }

instance WithVk Graphics.Vulkan.HL.Core.ApplicationInfo Graphics.Vulkan.ApplicationInfo where
  withVk a f =
    (wrapString (applicationName a) $
     wrapValue (Graphics.Vulkan.HL.Core.applicationVersion a) $
     wrapString (engineName a) $
     wrapValue (Graphics.Vulkan.HL.Core.engineVersion a) $
     wrapValue (Graphics.Vulkan.HL.Core.apiVersion a)
     f)
    (Graphics.Vulkan.ApplicationInfo StructureTypeApplicationInfo nullPtr)

data InstanceCreateInfo = InstanceCreateInfo { applicationInfo :: Graphics.Vulkan.HL.Core.ApplicationInfo
                                             , instanceLayers :: [LayerName]
                                             , instanceExtensions :: [ExtensionName]
                                             }

instance WithVk Graphics.Vulkan.HL.Core.InstanceCreateInfo Graphics.Vulkan.InstanceCreateInfo where
  withVk (Graphics.Vulkan.HL.Core.InstanceCreateInfo ai l e) f =
    (wrapInPtr ai $
     wrapArray l $
     wrapArray e
     f)
    (Graphics.Vulkan.InstanceCreateInfo StructureTypeInstanceCreateInfo nullPtr (Graphics.Vulkan.InstanceCreateFlags zeroBits))

data Instance = Instance Graphics.Vulkan.Instance
              deriving (Eq, Ord, Show)

instance FromVk Graphics.Vulkan.HL.Core.Instance Graphics.Vulkan.Instance where
  fromVk = return . Instance

data Surface = Surface Graphics.Vulkan.Surface
              deriving (Eq, Show)

instance FromVk Graphics.Vulkan.HL.Core.Surface Graphics.Vulkan.Surface where
  fromVk = return . Graphics.Vulkan.HL.Core.Surface

data Extension = Extension { extensionName :: ExtensionName
                           , extensionVersion :: Int
                           }
               deriving (Eq, Ord, Show, Read)

instance FromVk Extension Graphics.Vulkan.ExtensionProperties where
  fromVk (Graphics.Vulkan.ExtensionProperties name version) = do
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

instance FromVk Graphics.Vulkan.HL.Core.QueueFlags Graphics.Vulkan.QueueFlags where
  fromVk f =
    return $ Graphics.Vulkan.HL.Core.QueueFlags
      (f `hasFlag` QueueGraphicsBit)
      (f `hasFlag` QueueComputeBit)
      (f `hasFlag` QueueTransferBit)
      (f `hasFlag` QueueSparseBindingBit)

data QueueFamilyProperties = QueueFamilyProperties { queueFamily :: QueueFamily
                                                   , queueFlags :: Graphics.Vulkan.HL.Core.QueueFlags
                                                   , queueCount :: Int
                                                   }
                           deriving (Eq, Ord, Show)

instance FromVk Graphics.Vulkan.HL.Core.QueueFamilyProperties Graphics.Vulkan.QueueFamilyProperties where
  fromVk (Graphics.Vulkan.QueueFamilyProperties qf qc _ _) = do
    flags <- fromVk qf
    return $ Graphics.Vulkan.HL.Core.QueueFamilyProperties undefined flags (fromIntegral qc)

createInstance :: Graphics.Vulkan.HL.Core.InstanceCreateInfo -> IO Graphics.Vulkan.HL.Core.Instance
createInstance a =
  (wrapInPtr a $
   wrapConst nullPtr $
   wrapOutPtr id
  ) Graphics.Vulkan.createInstance

destroyInstance :: Graphics.Vulkan.HL.Core.Instance -> IO ()
destroyInstance (Instance i) = Graphics.Vulkan.destroyInstance i nullPtr

deviceExtensionProperties :: IO [Extension]
deviceExtensionProperties = wrapCountArray $ Graphics.Vulkan.enumerateInstanceExtensionProperties nullPtr

physicalDevices :: Graphics.Vulkan.HL.Core.Instance -> IO [Graphics.Vulkan.HL.Core.PhysicalDevice]
physicalDevices (Instance i) = wrapCountArray $ Graphics.Vulkan.enumeratePhysicalDevices i

queueFamilyProperties :: Graphics.Vulkan.HL.Core.PhysicalDevice -> IO [Graphics.Vulkan.HL.Core.QueueFamilyProperties]
queueFamilyProperties d@(PhysicalDevice h) =
  zipWith setFamily [0..] <$> wrapCountArray (Graphics.Vulkan.getPhysicalDeviceQueueFamilyProperties h)
  where setFamily a b = b { queueFamily = QueueFamily d a } :: Graphics.Vulkan.HL.Core.QueueFamilyProperties

createSurface :: Window -> Graphics.Vulkan.HL.Core.Instance -> IO Graphics.Vulkan.HL.Core.Surface
createSurface (Window w) (Instance i) =
  alloca (\ps -> do
             r <- createSurfaceFFI w i ps
             unless r $ error "SDL_CreateVulkanSurface failed"
             Graphics.Vulkan.HL.Core.Surface <$> peek ps)

queueFamilySupportsPresent :: QueueFamily -> Graphics.Vulkan.HL.Core.Surface -> IO Bool
queueFamilySupportsPresent (QueueFamily (PhysicalDevice d) qi) (Graphics.Vulkan.HL.Core.Surface s) =
  wrapOutPtr id $
  Graphics.Vulkan.getPhysicalDeviceSurfaceSupport d (fromIntegral qi) s

data QueueCreateInfo = QueueCreateInfo { queueCreateFamily :: QueueFamily
                                       , queueCreateCount :: Int
                                       }

instance WithVk QueueCreateInfo Graphics.Vulkan.DeviceQueueCreateInfo where
  withVk (QueueCreateInfo (QueueFamily _ i) c) f =
    f $ Graphics.Vulkan.DeviceQueueCreateInfo StructureTypeDeviceQueueCreateInfo nullPtr
    (Graphics.Vulkan.DeviceQueueCreateFlags zeroBits) (fromIntegral i) (fromIntegral c) nullPtr

data DeviceCreateInfo = DeviceCreateInfo { deviceQueues :: [QueueCreateInfo]
                                         , deviceLayers :: [LayerName]
                                         , deviceExtensions :: [ExtensionName]
                                         }

instance WithVk Graphics.Vulkan.HL.Core.DeviceCreateInfo Graphics.Vulkan.DeviceCreateInfo where
  withVk (Graphics.Vulkan.HL.Core.DeviceCreateInfo q l e) f =
    (wrapArray q $
     wrapArray l $
     wrapArray e $
     f . ($ nullPtr))
    (Graphics.Vulkan.DeviceCreateInfo StructureTypeDeviceCreateInfo nullPtr (Graphics.Vulkan.DeviceCreateFlags zeroBits))

data Device = Device Graphics.Vulkan.Device
            deriving (Eq, Ord, Show)

instance FromVk Graphics.Vulkan.HL.Core.Device Graphics.Vulkan.Device where
  fromVk = return . Device

createDevice :: Graphics.Vulkan.HL.Core.PhysicalDevice -> Graphics.Vulkan.HL.Core.DeviceCreateInfo -> IO Graphics.Vulkan.HL.Core.Device
createDevice (PhysicalDevice pd) a =
  wrapInPtr a
  (wrapConst nullPtr $
   wrapOutPtr id
  ) $ Graphics.Vulkan.createDevice pd

data Queue = Queue Graphics.Vulkan.Queue
           deriving (Eq, Ord, Show)

instance FromVk Graphics.Vulkan.HL.Core.Queue Graphics.Vulkan.Queue where
  fromVk = return . Queue

getQueue :: Graphics.Vulkan.HL.Core.Device -> QueueFamily -> Int -> IO Graphics.Vulkan.HL.Core.Queue
getQueue (Device d) (QueueFamily _ f) i =
  wrapOutPtr id
  $ Graphics.Vulkan.getDeviceQueue d (fromIntegral f) (fromIntegral i)

data CommandPoolCreateInfo = CommandPoolCreateInfo { comamndPoolQueueFamily :: QueueFamily
                                                   }

instance WithVk Graphics.Vulkan.HL.Core.CommandPoolCreateInfo Graphics.Vulkan.CommandPoolCreateInfo where
  withVk (Graphics.Vulkan.HL.Core.CommandPoolCreateInfo (QueueFamily _ qfi)) f =
    f $ Graphics.Vulkan.CommandPoolCreateInfo StructureTypeCommandPoolCreateInfo nullPtr
    zeroBits (fromIntegral qfi)

data CommandPool = CommandPool Graphics.Vulkan.CommandPool
                 deriving (Eq, Ord, Show)

instance FromVk Graphics.Vulkan.HL.Core.CommandPool Graphics.Vulkan.CommandPool where
  fromVk = return . Graphics.Vulkan.HL.Core.CommandPool

createCommandPool :: Graphics.Vulkan.HL.Core.Device -> Graphics.Vulkan.HL.Core.CommandPoolCreateInfo -> IO Graphics.Vulkan.HL.Core.CommandPool
createCommandPool (Device d) ci =
  wrapInPtr ci
  (wrapConst nullPtr $
   wrapOutPtr id)
  $ Graphics.Vulkan.createCommandPool d

data SurfaceFormat = SurfaceFormat Graphics.Vulkan.SurfaceFormat
                   deriving (Eq, Ord, Show)

instance FromVk Graphics.Vulkan.HL.Core.SurfaceFormat Graphics.Vulkan.SurfaceFormat where
  fromVk = return . Graphics.Vulkan.HL.Core.SurfaceFormat

surfaceFormats :: Graphics.Vulkan.HL.Core.PhysicalDevice -> Graphics.Vulkan.HL.Core.Surface -> IO [Graphics.Vulkan.HL.Core.SurfaceFormat]
surfaceFormats (PhysicalDevice pd) (Graphics.Vulkan.HL.Core.Surface s) =
  wrapCountArray $ Graphics.Vulkan.getPhysicalDeviceSurfaceFormats pd s
