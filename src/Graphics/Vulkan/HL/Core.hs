{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeSynonymInstances #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.HL.Core where

import Control.Monad
import Data.Bits
import Data.Int
import Data.Vector.Storable.Sized hiding (head, length)
import Data.Void
import Data.Word
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import qualified Graphics.Vulkan as Vk
import qualified Graphics.Vulkan.KHR.Surface as VkSu
import Graphics.Vulkan.HL.Internal.Marshal
import SDL.Internal.Types (Window(Window))
import SDL.Video.Vulkan

type LayerName = String
type ExtensionName = String
data Version = Version Int Int Int

instance WithVk Version Word32 where
  withVk (Version a b c) f = f v
    where v = Vk.makeVersion (fromIntegral a) (fromIntegral b) (fromIntegral c)

data ApplicationInfo = ApplicationInfo { applicationName :: String
                                       , applicationVersion :: Version
                                       , engineName :: String
                                       , engineVersion :: Version
                                       , apiVersion :: Version
                                       }

instance WithVk ApplicationInfo Vk.ApplicationInfo where
  withVk a f =
    (wrapString (applicationName a) $
     wrapValue (applicationVersion a) $
     wrapString (engineName a) $
     wrapValue (engineVersion a) $
     wrapValue (apiVersion a)
     f)
    (Vk.ApplicationInfo Vk.StructureTypeApplicationInfo nullPtr)

data InstanceCreateInfo = InstanceCreateInfo { applicationInfo :: ApplicationInfo
                                             , instanceLayers :: [LayerName]
                                             , instanceExtensions :: [ExtensionName]
                                             }

instance WithVk InstanceCreateInfo Vk.InstanceCreateInfo where
  withVk (InstanceCreateInfo ai l e) f =
    (wrapInPtr ai $
     wrapInArray l $
     wrapInArray e
     f)
    (Vk.InstanceCreateInfo Vk.StructureTypeInstanceCreateInfo nullPtr (Vk.InstanceCreateFlags zeroBits))

data Extension = Extension { extensionName :: ExtensionName
                           , extensionVersion :: Int
                           }
               deriving (Eq, Ord, Show, Read)

instance FromVk Extension Vk.ExtensionProperties where
  fromVk (Vk.ExtensionProperties name version) = do
    let nameList = Prelude.reverse (Data.Vector.Storable.Sized.foldl' (flip (:)) [] name)
    withArray nameList (\pname -> do
                           n <- peekCString pname
                           return $ Extension n (fromIntegral version)
                       )

hasFlag :: Bits a => a -> a -> Bool
hasFlag a b = a .|. b /= zeroBits

createInstance :: InstanceCreateInfo -> IO Vk.Instance
createInstance a =
  (wrapInPtr a $
   wrapConst nullPtr $
   wrapOutPtr id
  ) Vk.createInstance

destroyInstance :: Vk.Instance -> IO ()
destroyInstance i = Vk.destroyInstance i nullPtr

deviceExtensionProperties :: IO [Extension]
deviceExtensionProperties = wrapCountArray $ Vk.enumerateInstanceExtensionProperties nullPtr

physicalDevices :: Vk.Instance -> IO [Vk.PhysicalDevice]
physicalDevices i = wrapCountArray $ Vk.enumeratePhysicalDevices i

queueFamilyProperties :: Vk.PhysicalDevice -> IO [Vk.QueueFamilyProperties]
queueFamilyProperties pd =
  wrapCountArray (Vk.getPhysicalDeviceQueueFamilyProperties pd)

createSurface :: Window -> Vk.Instance -> IO Vk.Surface
createSurface (Window w) i =
  alloca (\ps -> do
             r <- createSurfaceFFI w i ps
             unless r $ error "SDL_CreateVulkanSurface failed"
             peek ps)

queueFamilySupportsPresent :: Vk.PhysicalDevice -> Int -> Vk.Surface -> IO Bool
queueFamilySupportsPresent pd qi s =
  wrapOutPtr id $
  Vk.getPhysicalDeviceSurfaceSupport pd (fromIntegral qi) s

data QueueCreateInfo = QueueCreateInfo { queueCreateFamily :: Int
                                       , queuePriorities :: [Float]
                                       }

instance WithVk QueueCreateInfo Vk.DeviceQueueCreateInfo where
  withVk (QueueCreateInfo i c) f =
    wrapInArray c
    f $ Vk.DeviceQueueCreateInfo Vk.StructureTypeDeviceQueueCreateInfo nullPtr
    (Vk.DeviceQueueCreateFlags zeroBits) (fromIntegral i)

data DeviceCreateInfo = DeviceCreateInfo { deviceQueues :: [QueueCreateInfo]
                                         , deviceLayers :: [LayerName]
                                         , deviceExtensions :: [ExtensionName]
                                         }

instance WithVk DeviceCreateInfo Vk.DeviceCreateInfo where
  withVk (DeviceCreateInfo q l e) f =
    (wrapInArray q $
     wrapInArray l $
     wrapInArray e $
     f . ($ nullPtr))
    (Vk.DeviceCreateInfo Vk.StructureTypeDeviceCreateInfo nullPtr (Vk.DeviceCreateFlags zeroBits))

createDevice :: Vk.PhysicalDevice -> DeviceCreateInfo -> IO Vk.Device
createDevice pd a =
  wrapInPtr a
  (wrapConst nullPtr $
   wrapOutPtr id
  ) $ Vk.createDevice pd

getQueue :: Vk.Device -> Int -> Int -> IO Vk.Queue
getQueue d f i =
  wrapOutPtr id
  $ Vk.getDeviceQueue d (fromIntegral f) (fromIntegral i)

data CommandPoolCreateInfo = CommandPoolCreateInfo { comamndPoolQueueFamily :: Int
                                                   }

instance WithVk CommandPoolCreateInfo Vk.CommandPoolCreateInfo where
  withVk (CommandPoolCreateInfo qfi) f =
    f $ Vk.CommandPoolCreateInfo Vk.StructureTypeCommandPoolCreateInfo nullPtr
    zeroBits (fromIntegral qfi)

createCommandPool :: Vk.Device -> CommandPoolCreateInfo -> IO Vk.CommandPool
createCommandPool d ci =
  wrapInPtr ci
  (wrapConst nullPtr $
   wrapOutPtr id)
  $ Vk.createCommandPool d

surfaceFormats :: Vk.PhysicalDevice -> Vk.Surface -> IO [Vk.SurfaceFormat]
surfaceFormats pd s =
  wrapCountArray $ Vk.getPhysicalDeviceSurfaceFormats pd s

surfaceCapabilities :: Vk.PhysicalDevice -> Vk.Surface -> IO Vk.SurfaceCapabilities
surfaceCapabilities pd s =
  wrapOutPtr id $ Vk.getPhysicalDeviceSurfaceCapabilities pd s

allocateCommandBuffers :: Vk.Device -> Vk.CommandPool -> Vk.CommandBufferLevel -> Int -> IO [Vk.CommandBuffer]
allocateCommandBuffers d cp l n =
  wrapInPtr (Vk.CommandBufferAllocateInfo Vk.StructureTypeCommandBufferAllocateInfo nullPtr cp l (fromIntegral n))
  (wrapOutArray n id)
  $ Vk.allocateCommandBuffers d

allocateCommandBuffer :: Vk.Device -> Vk.CommandPool -> Vk.CommandBufferLevel -> IO Vk.CommandBuffer
allocateCommandBuffer d cp l = head <$> allocateCommandBuffers d cp l 1

data SwapchainCreateInfo = SwapchainCreateInfo { flags :: Vk.SwapchainCreateFlags
                                               , surface :: Vk.Surface
                                               , minImageCount :: Int
                                               , imageFormat :: Vk.SurfaceFormat
                                               , imageExtent :: Vk.Extent2D
                                               , imageArrayLayers :: Int
                                               , imageUsage :: Vk.ImageUsageFlags
                                               , imageSharingMode :: Vk.SharingMode
                                               , queueFamilyIndices :: [Int]
                                               , preTransform :: Vk.SurfaceTransformFlags
                                               , compositeAlpha :: Vk.CompositeAlphaFlags
                                               , presentMode :: Vk.PresentMode
                                               , clipped :: Bool
                                               }

instance WithVk SwapchainCreateInfo Vk.SwapchainCreateInfo where
  withVk s f =
    (wrapConst (flags (s :: SwapchainCreateInfo)) $
     wrapValue (surface s) $
     wrapValue (minImageCount s) $
     wrapConst (VkSu.format $ imageFormat s) $
     wrapConst (Vk.colorSpace $ imageFormat s) $
     wrapConst (imageExtent s) $
     wrapValue (imageArrayLayers s) $
     wrapConst (imageUsage s) $
     wrapConst (imageSharingMode s) $
     wrapInArray (queueFamilyIndices s) $
     wrapConst (preTransform s) $
     wrapConst (compositeAlpha s) $
     wrapConst (presentMode s) $
     wrapValue (clipped s) $
     wrapConst (Vk.Swapchain 0)
     f)
    (Vk.SwapchainCreateInfo (Vk.StructureType 1000001000) nullPtr)

createSwapchain :: Vk.Device -> SwapchainCreateInfo -> IO Vk.Swapchain
createSwapchain d ci =
  wrapInPtr ci
  (wrapConst nullPtr $
   wrapOutPtr id)
  $ Vk.createSwapchain d

swapchainImages :: Vk.Device -> Vk.Swapchain -> IO [Vk.Image]
swapchainImages d s = wrapCountArray $ Vk.getSwapchainImages d s

type FN_DebugReportCallback =
  (Vk.DebugReportFlags ->
     Vk.DebugReportObjectType ->
       Word64 ->
         CSize -> Int32 -> Ptr CChar -> Ptr CChar -> Ptr Void -> IO Vk.Bool32)

type DebugReportCallbackFun =
  (Vk.DebugReportFlags ->
     Vk.DebugReportObjectType ->
       Word64 ->
         CSize -> Int32 -> String -> String -> IO Bool)

foreign import ccall "wrapper" mkDebugReportCallback :: FN_DebugReportCallback -> IO Vk.PFN_vkDebugReportCallbackEXT

data DebugReportCallback = DebugReportCallback Vk.DebugReportCallback Vk.PFN_vkDebugReportCallbackEXT
                         deriving (Eq, Ord, Show)

type FN_createDebugReportCallback =
  Vk.Instance ->
  Ptr Vk.DebugReportCallbackCreateInfo ->
    Ptr Vk.AllocationCallbacks -> Ptr Vk.DebugReportCallback -> IO Vk.Result

createDebugReportCallback :: Vk.Instance -> Vk.DebugReportFlags -> DebugReportCallbackFun -> IO DebugReportCallback
createDebugReportCallback i flags cb = do
  cbPtr <- mkDebugReportCallback wrappedCallback
  let ci = Vk.DebugReportCallbackCreateInfo (Vk.StructureType 1000011000) nullPtr flags cbPtr nullPtr
  drc <- (wrapInPtr (ci :: Vk.DebugReportCallbackCreateInfo) $
          wrapConst nullPtr $
          wrapOutPtr id)
    $ Vk.createDebugReportCallback i
  return $ DebugReportCallback drc cbPtr
  where
    wrappedCallback :: FN_DebugReportCallback
    wrappedCallback f ot o l mc lp m _ = do
      r <- join $ cb f ot o l mc <$> peekCString lp <*> peekCString m
      return $ Vk.Bool32 $ if r then 1 else 0

data ImageViewCreateInfo = ImageViewCreateInfo { flags :: Vk.ImageViewCreateFlags
                                               , image :: Vk.Image
                                               , viewType :: Vk.ImageViewType
                                               , format :: Vk.Format
                                               , components :: Vk.ComponentMapping
                                               , subresourceRange :: Vk.ImageSubresourceRange
                                               }

instance WithVk ImageViewCreateInfo Vk.ImageViewCreateInfo where
  withVk s f =
    (wrapConst (flags (s :: ImageViewCreateInfo)) $
     wrapConst (image s) $
     wrapConst (viewType s) $
     wrapConst (format s) $
     wrapConst (components s) $
     wrapConst (subresourceRange s)
     f)
    (Vk.ImageViewCreateInfo Vk.StructureTypeImageViewCreateInfo nullPtr)

createImageView :: Vk.Device -> ImageViewCreateInfo -> IO Vk.ImageView
createImageView d ci =
  wrapInPtr ci
  (wrapConst nullPtr $
   wrapOutPtr id)
  $ Vk.createImageView d

data SubpassDescription = SubpassDescription { flags :: Vk.SubpassDescriptionFlags
                                             , pipelineBindPoint :: Vk.PipelineBindPoint
                                             , inputAttachments :: [Vk.AttachmentReference]
                                             , colorAttachments :: [Vk.AttachmentReference]
                                             -- , pResolveAttachments :: Maybe [AttachmentReference]
                                             -- , depthStencilAttachment :: Vk.AttachmentReference
                                             , preserveAttachments :: [Int]
                                             }

instance WithVk SubpassDescription Vk.SubpassDescription where
  withVk s f =
    (wrapConst (flags (s :: SubpassDescription)) $
     wrapConst (pipelineBindPoint s) $
     wrapInArray (inputAttachments s) $
     wrapInArray (colorAttachments s) $
     wrapConst nullPtr $ -- TODO: hook up resolve
     wrapConst nullPtr $ -- TODO: hook up depth
     wrapInArray (preserveAttachments s)
     f)
    (Vk.SubpassDescription)

data RenderPassCreateInfo = RenderPassCreateInfo { flags :: Vk.RenderPassCreateFlags
                                                 , attachments :: [Vk.AttachmentDescription]
                                                 , subpasses :: [SubpassDescription]
                                                 , dependencies :: [Vk.SubpassDependency]
                                                 }

instance WithVk RenderPassCreateInfo Vk.RenderPassCreateInfo where
  withVk s f =
    (wrapConst (flags (s :: RenderPassCreateInfo)) $
     wrapInArray (attachments s) $
     wrapInArray (subpasses s) $
     wrapInArray (dependencies s)
     f)
    (Vk.RenderPassCreateInfo Vk.StructureTypeRenderPassCreateInfo nullPtr)

createRenderPass :: Vk.Device -> RenderPassCreateInfo -> IO Vk.RenderPass
createRenderPass d ci =
  wrapInPtr ci
  (wrapConst nullPtr $
   wrapOutPtr id)
  $ Vk.createRenderPass d
