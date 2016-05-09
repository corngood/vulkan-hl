{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}

module Graphics.Vulkan.HL.Internal.Marshal where

import Data.Word
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Graphics.Vulkan

import Graphics.Vulkan.HL.Internal.Orphan ()

class Checkable a where
  check :: a -> IO ()

instance Checkable () where
  check = return

instance Checkable Result where
  check Success = return ()
  check a = error $ show a

class FromVk a b | a -> b where
  fromVk :: b -> IO a

instance FromVk Bool Bool32 where
  fromVk = return . (\(Bool32 b) -> b /= 0)

instance FromVk SurfaceFormat SurfaceFormat where
  fromVk = return

instance FromVk SurfaceCapabilities SurfaceCapabilities where
  fromVk = return

instance FromVk Instance Instance where
  fromVk = return

instance FromVk PhysicalDevice PhysicalDevice where
  fromVk = return

instance FromVk QueueFamilyProperties QueueFamilyProperties where
  fromVk = return

instance FromVk Device Device where
  fromVk = return

instance FromVk CommandBuffer CommandBuffer where
  fromVk = return

instance FromVk Swapchain Swapchain where
  fromVk = return

instance FromVk Image Image where
  fromVk = return

instance FromVk Queue Queue where
  fromVk = return

instance FromVk CommandPool CommandPool where
  fromVk = return

instance FromVk ImageView ImageView where
  fromVk = return

instance FromVk RenderPass RenderPass where
  fromVk = return

instance FromVk DebugReportCallback DebugReportCallback where
  fromVk = return

class WithVk a b | a -> b where
  withVk :: a -> (b -> IO c) -> IO c

instance WithVk String CString where
  withVk = withCString

instance WithVk CommandBufferAllocateInfo CommandBufferAllocateInfo where
  withVk a f = f a

instance WithVk Surface Surface where
  withVk a f = f a

instance WithVk AttachmentReference AttachmentReference where
  withVk a f = f a

instance WithVk AttachmentDescription AttachmentDescription where
  withVk a f = f a

instance WithVk SubpassDependency SubpassDependency where
  withVk a f = f a

instance WithVk DebugReportCallbackCreateInfo DebugReportCallbackCreateInfo where
  withVk a f = f a

instance WithVk Int Word32 where
  withVk a f = f (fromIntegral a)

instance WithVk Bool Bool32 where
  withVk a f = f (Bool32 $ if a then 1 else 0)

instance WithVk Float CFloat where
  withVk a f = f (CFloat a)

withList :: WithVk a b => [a] -> ([b] -> IO c) -> IO c
withList a f =
  member (reverse a) []
  where
    member [] l = f l
    member (x:xs) l = withVk x (\y -> member xs (y:l))

wrapCountArray :: (Storable b, FromVk a b, Integral c, Storable c, Checkable d) => (Ptr c -> Ptr b -> IO d) -> IO [a]
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

wrapOutPtr :: (Storable b, FromVk a b, Checkable d) => (c -> IO d) -> (Ptr b -> c) -> IO a
wrapOutPtr g f =
  alloca (\ptr -> do
           d <- g $ f ptr
           check d
           a <- peek ptr
           fromVk a
         )

wrapInArray :: (Num l, WithVk a b, Storable b) => [a] -> (c -> IO d) -> (l -> Ptr b -> c) -> IO d
wrapInArray a g f = withList a (`withArrayLen` (\l p -> g $ f (fromIntegral l) p))

wrapOutArray :: (Storable b, FromVk a b, Checkable d) => Int -> (c -> IO d) -> (Ptr b -> c) -> IO [a]
wrapOutArray n g f =
  allocaArray n (\ptr -> do
                    d <- g $ f ptr
                    check d
                    a <- peekArray n ptr
                    mapM fromVk a
                )
