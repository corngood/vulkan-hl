{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language DuplicateRecordFields #-}
{-# language StandaloneDeriving #-}
{-# options_ghc -fno-warn-orphans #-}

module Lib where

import Control.Monad
import Data.Bits
import Data.Vector.Storable.Sized
import Data.Word
import Foreign.C.String
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Graphics.Vulkan
import SDL.Internal.Types (Window(Window))
import SDL.Video.Vulkan

deriving instance Show VkSurfaceKHR
deriving instance Show VkExtent3D
deriving instance Show VkQueueFamilyProperties

class FromVk a b | a -> b where
  fromVk :: a -> IO b

class WithVk a b | a -> b where
  withVk :: a -> (b -> IO c) -> IO c

instance WithVk String CString where
  withVk = withCString

withList :: WithVk a b => [a] -> ([b] -> IO c) -> IO c
withList a f =
  member (reverse a) []
  where
    member [] l = f l
    member (x:xs) l = withVk x (\y -> member xs (y:l))

instance FromVk VkBool32 Bool where
  fromVk = return . (\(VkBool32 b) -> b /= 0)

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

data InstanceCreateInfo = InstanceCreateInfo { applicationInfo :: ApplicationInfo
                                             , enabledLayers :: [LayerName]
                                             , enabledExtensions :: [ExtensionName]
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

instance WithVk InstanceCreateInfo VkInstanceCreateInfo where
  withVk a f =
    (wrapInPtr (applicationInfo a) $
     wrapArray (enabledLayers a) $
     wrapArray (enabledExtensions a)
     f)
    (VkInstanceCreateInfo VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO nullPtr (VkInstanceCreateFlags zeroBits))

data Instance = Instance VkInstance
              deriving (Eq, Ord, Show)

instance FromVk VkInstance Instance where
  fromVk = return . Instance

data Surface = Surface VkSurfaceKHR
              deriving (Eq, Show)

-- instance Show Surface where show (Surface (VkSurfaceKHR w)) = "Surface " ++ show w

instance FromVk VkSurfaceKHR Surface where
  fromVk = return . Surface

data Extension = Extension { extensionName :: ExtensionName
                           , extensionVersion :: Int
                           }
               deriving (Eq, Ord, Show, Read)

instance FromVk VkExtensionProperties Extension where
  fromVk (VkExtensionProperties name version) = do
    let nameList = Prelude.reverse (Data.Vector.Storable.Sized.foldl' (flip (:)) [] name)
    withArray nameList (\pname -> do
                           n <- peekCString pname
                           return $ Extension n (fromIntegral version)
                       )

data PhysicalDevice = PhysicalDevice VkPhysicalDevice
                    deriving (Eq, Ord, Show)

instance FromVk VkPhysicalDevice PhysicalDevice where
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

instance FromVk VkQueueFlags QueueFlags where
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

instance FromVk VkQueueFamilyProperties QueueFamilyProperties where
  fromVk (VkQueueFamilyProperties qf qc _ _) = do
    flags <- fromVk qf
    return $ QueueFamilyProperties undefined flags (fromIntegral qc)

class Checkable a where
  check :: a -> IO ()

instance Checkable () where
  check = return

instance Checkable VkResult where
  check VK_SUCCESS = return ()
  check a = error $ show a

wrapCountArray :: (Storable a, FromVk a b, Integral c, Storable c, Checkable d) => (Ptr c -> Ptr a -> IO d) -> IO [b]
wrapCountArray f =
  with 0 (\pcount -> do
             f pcount nullPtr >>= check

             count <- fromIntegral <$> peek pcount
             allocaArray count (\parray -> do
                                   f pcount parray >>= check
                                   r <- peekArray count parray
                                   sequence $ fromVk <$> r
                               )
         )

wrapString :: String -> (c -> IO d) -> (CString -> c) -> IO d
wrapString a g f = withCString a (g . f)

wrapConst :: a -> (c -> IO d) -> (a -> c) -> IO d
wrapConst a g f = (g . f) a

wrapValue :: (WithVk a b) => a -> (c -> IO d) -> (b -> c) -> IO d
wrapValue a g f = withVk a (g . f)

wrapInPtr :: (WithVk a b, Storable b) => a -> (c -> IO d) -> (Ptr b -> c) -> IO d
wrapInPtr a g f = withVk a (`with` (g . f))

wrapArray :: (Num l, WithVk a b, Storable b) => [a] -> (c -> IO d) -> (l -> Ptr b -> c) -> IO d
wrapArray a g f = withList a (`withArrayLen` (\l p -> g $ f (fromIntegral l) p))

wrapOutPtr :: (Storable a, FromVk a b) => (c -> IO VkResult) -> (Ptr a -> c) -> IO b
wrapOutPtr g f =
  alloca (\ptr -> do
           d <- g $ f ptr
           check d
           a <- peek ptr
           fromVk a
         )

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
  where setFamily a b = b { queueFamily = QueueFamily d a }

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
