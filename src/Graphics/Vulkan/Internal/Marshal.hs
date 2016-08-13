{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language GeneralizedNewtypeDeriving #-}

module Graphics.Vulkan.Internal.Marshal where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int
import Data.Vector.Storable.Sized (Vector, toList)
import Data.Void
import Data.Word
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Graphics.Vulkan.Raw

class Checkable a where
  check :: a -> IO ()

instance Checkable () where
  check = pure

instance Checkable VkResult where
  check VK_SUCCESS = pure ()
  check a = error $ show a

class FromVk a b | a -> b where
  fromVk :: b -> IO a

instance FromVk Bool VkBool32 where
  fromVk = pure . (\(VkBool32 b) -> b /= 0)

instance FromVk Word Word32 where
  fromVk = pure . fromIntegral

instance FromVk Int Int32 where
  fromVk = pure . fromIntegral

instance FromVk VkCommandBuffer VkCommandBuffer where fromVk = pure
instance FromVk VkCommandPool VkCommandPool where fromVk = pure
instance FromVk VkDebugReportCallbackEXT VkDebugReportCallbackEXT where fromVk = pure
instance FromVk VkDevice VkDevice where fromVk = pure
instance FromVk VkFramebuffer VkFramebuffer where fromVk = pure
instance FromVk VkImage VkImage where fromVk = pure
instance FromVk VkImageView VkImageView where fromVk = pure
instance FromVk VkInstance VkInstance where fromVk = pure
instance FromVk VkPhysicalDevice VkPhysicalDevice where fromVk = pure
instance FromVk VkQueue VkQueue where fromVk = pure
instance FromVk VkQueueFamilyProperties VkQueueFamilyProperties where fromVk = pure
instance FromVk VkRenderPass VkRenderPass where fromVk = pure
instance FromVk VkSemaphore VkSemaphore where fromVk = pure
instance FromVk VkSurfaceCapabilitiesKHR VkSurfaceCapabilitiesKHR where fromVk = pure
instance FromVk VkSurfaceFormatKHR VkSurfaceFormatKHR where fromVk = pure
instance FromVk VkSwapchainKHR VkSwapchainKHR where fromVk = pure

class WithVk a b | a -> b where
  withVk :: a -> (b -> IO c) -> IO c

instance WithVk String CString where
  withVk = withCString

instance WithVk VkCommandBufferAllocateInfo VkCommandBufferAllocateInfo where withVk a f = f a
instance WithVk VkSurfaceKHR VkSurfaceKHR where withVk a f = f a
instance WithVk VkAttachmentReference VkAttachmentReference where withVk a f = f a
instance WithVk VkAttachmentDescription VkAttachmentDescription where withVk a f = f a
instance WithVk VkSubpassDependency VkSubpassDependency where withVk a f = f a
instance WithVk VkDebugReportCallbackCreateInfoEXT VkDebugReportCallbackCreateInfoEXT where withVk a f = f a
instance WithVk VkSemaphoreCreateInfo VkSemaphoreCreateInfo where withVk a f = f a
instance WithVk VkCommandBufferBeginInfo VkCommandBufferBeginInfo where withVk a f = f a
instance WithVk VkImageView VkImageView where withVk a f = f a
instance WithVk VkMemoryBarrier VkMemoryBarrier where withVk a f = f a
instance WithVk VkBufferMemoryBarrier VkBufferMemoryBarrier where withVk a f = f a
instance WithVk VkImageMemoryBarrier VkImageMemoryBarrier where withVk a f = f a
instance WithVk VkClearValue VkClearValue where withVk a f = f a
instance WithVk VkSemaphore VkSemaphore where withVk a f = f a
instance WithVk VkPipelineStageFlags VkPipelineStageFlags where withVk a f = f a
instance WithVk VkCommandBuffer VkCommandBuffer where withVk a f = f a
instance WithVk VkSwapchainKHR VkSwapchainKHR where withVk a f = f a

instance WithVk Word Word32 where
  withVk a f = f (fromIntegral a)

instance WithVk Int Int32 where
  withVk a f = f (fromIntegral a)

instance WithVk Bool VkBool32 where
  withVk a f = f (VkBool32 $ if a then 1 else 0)

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

wrapOptPtr :: (WithVk a b, Storable b) => Maybe a -> (c -> IO d) -> (Ptr b -> c) -> IO d
wrapOptPtr (Just a) g f = wrapInPtr a g f
wrapOptPtr Nothing g f = g $ f nullPtr

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

wrapInArrayNoCount :: (WithVk a b, Storable b) => [a] -> (c -> IO d) -> (Ptr b -> c) -> IO d
wrapInArrayNoCount a g f = withList a (`withArray` (g . f))

wrapOptArrayNoCount :: (WithVk a b, Storable b) => Maybe [a] -> (c -> IO d) -> (Ptr b -> c) -> IO d
wrapOptArrayNoCount (Just a) g f = wrapInArrayNoCount a g f
wrapOptArrayNoCount Nothing g f = g $ f nullPtr

wrapOutArray :: (Storable b, FromVk a b, Checkable d) => Word -> (c -> IO d) -> (Ptr b -> c) -> IO [a]
wrapOutArray n g f =
  allocaArray (fromIntegral n) (\ptr -> do
                    d <- g $ f ptr
                    check d
                    a <- peekArray (fromIntegral n) ptr
                    mapM fromVk a
                )

wrapByteString :: Num l => ByteString -> (c -> IO d) -> (l -> Ptr b -> c) -> IO d
wrapByteString a g f = unsafeUseAsCStringLen a (\(p, l) -> g $ f (fromIntegral l) $ castPtr p)

fromVector :: (FromVk a b, Storable b) => Vector n b -> IO [a]
fromVector = mapM fromVk . toList

fromLenVector :: (FromVk a b, Storable b) => Word32 -> Vector n b -> IO [a]
fromLenVector l a = take (fromIntegral l) <$> fromVector a

fromArray :: (FromVk a b, Storable b) => Word32 -> Ptr b -> IO [a]
fromArray l a = peekArray (fromIntegral l) a >>= mapM fromVk

newtype Handle b = Handle b deriving(Eq, Ord, Show)
instance FromVk (Handle b) b where fromVk = pure . Handle
instance WithVk (Handle b) b where withVk (Handle a) f = f a

newtype Enumerator b = Enumerator b deriving(Eq, Ord, Show, Read)
instance FromVk (Enumerator b) b where fromVk = pure . Enumerator
instance WithVk (Enumerator b) b where withVk (Enumerator a) f = f a

newtype Flags b = Flags b deriving(Eq, Ord, Show, Read, Bits)
instance FromVk (Flags b) b where fromVk = pure . Flags
instance WithVk (Flags b) b where withVk (Flags a) f = f a

instance FromVk (Ptr Void) (Ptr Void) where fromVk = pure
