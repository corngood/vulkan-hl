{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}

module Graphics.Vulkan.HL.Internal.Marshal where

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

instance Checkable VkResult where
  check VK_SUCCESS = return ()
  check a = error $ show a

class FromVk a b | a -> b where
  fromVk :: b -> IO a

instance FromVk Bool VkBool32 where
  fromVk = return . (\(VkBool32 b) -> b /= 0)

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

wrapArray :: (Num l, WithVk a b, Storable b) => [a] -> (c -> IO d) -> (l -> Ptr b -> c) -> IO d
wrapArray a g f = withList a (`withArrayLen` (\l p -> g $ f (fromIntegral l) p))

wrapOutPtr :: (Storable b, FromVk a b, Checkable d) => (c -> IO d) -> (Ptr b -> c) -> IO a
wrapOutPtr g f =
  alloca (\ptr -> do
           d <- g $ f ptr
           check d
           a <- peek ptr
           fromVk a
         )
