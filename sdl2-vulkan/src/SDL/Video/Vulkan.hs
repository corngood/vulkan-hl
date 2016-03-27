module SDL.Video.Vulkan
       ( requiredInstanceExtensions
       , createSurfaceFFI
       )
       where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Graphics.Vulkan
import SDL.Raw.Types

foreign import ccall "SDL_vulkan.h SDL_GetVulkanInstanceExtensions" getInstanceExtensionsFFI :: Ptr CUInt -> Ptr (Ptr CChar) -> IO Bool

requiredInstanceExtensions :: IO [String]
requiredInstanceExtensions =
    with 0 (\pcount -> do
               getInstanceExtensionsFFI pcount nullPtr

               count <- fromIntegral <$> peek pcount
               allocaArray count (\pext -> do
                                     getInstanceExtensionsFFI pcount pext
                                     r <- peekArray count pext
                                     sequence $ peekCString <$> r
                                 )
           )

foreign import ccall "SDL_vulkan.h SDL_CreateVulkanSurface" createSurfaceFFI :: Window -> VkInstance -> Ptr VkSurfaceKHR -> IO Bool
