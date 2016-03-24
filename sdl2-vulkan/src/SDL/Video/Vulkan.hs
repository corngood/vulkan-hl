module SDL.Video.Vulkan
       ( instanceExtensions
       )
       where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "SDL_vulkan.h SDL_GetVulkanInstanceExtensions" getVulkanInstanceExtensionsFFI :: Ptr CUInt -> Ptr (Ptr CChar) -> IO Bool

instanceExtensions :: IO [String]
instanceExtensions =
    with 0 (\pcount -> do
               getVulkanInstanceExtensionsFFI pcount nullPtr

               count <- fromIntegral <$> peek pcount
               allocaArray count (\pext -> do
                                     getVulkanInstanceExtensionsFFI pcount pext
                                     r <- peekArray count pext
                                     sequence $ peekCString <$> r
                                 )
           )
