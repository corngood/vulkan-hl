{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language DuplicateRecordFields #-}

module Lib where

import Data.Bits
import Data.Vector.Storable.Sized
import Foreign.C.String
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Graphics.Vulkan

class FromVk a b | a -> b where
  fromVk :: a -> IO b

class WithVk a b | a -> b where
  withVk :: a -> (Ptr b -> IO c) -> IO c

type LayerName = String
type ExtensionName = String
data Version = Version Int Int Int

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
    withCString (applicationName a)
    (\namePtr ->
      with (VkApplicationInfo
            VK_STRUCTURE_TYPE_APPLICATION_INFO
            nullPtr namePtr 1 namePtr 0 (vkMakeVersion 1 0 3)
           ) f
    )

instance WithVk InstanceCreateInfo VkInstanceCreateInfo where
  withVk a f = wrapInPtr (applicationInfo a) (\g -> with (g 0 nullPtr 0 nullPtr) f) $
    VkInstanceCreateInfo VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO nullPtr (VkInstanceCreateFlags zeroBits)

data Instance = Instance VkInstance
              deriving (Eq, Ord, Show)

instance FromVk VkInstance Instance where
  fromVk = return . Instance

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

wrapCountArray :: (Storable a, FromVk a b, Integral c, Storable c) => (Ptr c -> Ptr a -> IO VkResult) -> IO [b]
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

wrapInPtr :: WithVk a b => a -> (c -> IO d) -> (Ptr b -> c) -> IO d
wrapInPtr a g f = withVk a (g . f)

wrapString :: String -> (c -> IO d) -> (CString -> c) -> IO d
wrapString a g f = withCString a (g . f)

wrapOutPtr :: (Storable a, FromVk a b) => (c -> IO VkResult) -> (Ptr a -> c) -> IO b
wrapOutPtr g f =
  alloca (\ptr -> do
           d <- g $ f ptr
           check d
           a <- peek ptr
           fromVk a
         )

createInstance :: InstanceCreateInfo -> IO Instance
createInstance a = wrapInPtr a (wrapOutPtr id . ($ nullPtr)) vkCreateInstance

deviceExtensionProperties :: IO [Extension]
deviceExtensionProperties = wrapCountArray $ vkEnumerateInstanceExtensionProperties nullPtr

check :: VkResult -> IO ()
check VK_SUCCESS = return ()
check a = error $ show a
