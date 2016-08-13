{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeSynonymInstances #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# options_ghc -Wno-name-shadowing -Wno-orphans #-}

module Graphics.Vulkan.Core where

import Control.Monad
import Data.Bits
import Data.Default
import Data.Vector.Storable.Sized as V hiding (head, length)
import Data.Void
import Data.Word
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Graphics.Vulkan.Raw
import SDL.Internal.Types (Window(Window))
import SDL.Video.Vulkan

import Graphics.Vulkan.Internal.Marshal
import Graphics.Vulkan.Types

instance Default VkFence where def = VkFence 0
instance Default VkShaderModule where def = VkShaderModule 0
instance Default VkPipeline where def = VkPipeline 0

nullHandle :: Default a => Handle a
nullHandle = Handle def

instance WithVk Version Word32 where
  withVk (Version a b c) f = f v
    where v = vkMakeVersion (fromIntegral a) (fromIntegral b) (fromIntegral c)

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
  withVk (InstanceCreateInfo ai l e) f =
    (wrapInPtr ai $
     wrapInArray l $
     wrapInArray e
     f)
    (VkInstanceCreateInfo VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO nullPtr (VkInstanceCreateFlags zeroBits))

instance FromVk Extension VkExtensionProperties where
  fromVk (VkExtensionProperties name version) = do
    let nameList = Prelude.reverse (V.foldl' (flip (:)) [] name)
    withArray nameList (\pname -> do
                           n <- peekCString pname
                           pure $ Extension n (fromIntegral version)
                       )

hasFlags :: Bits a => a -> a -> Bool
hasFlags a b = a .&. b /= zeroBits

createInstance :: InstanceCreateInfo -> IO Instance
createInstance a =
  (wrapInPtr a $
   wrapConst nullPtr $
   wrapOutPtr id
  ) vkCreateInstance

destroyInstance :: Instance -> IO ()
destroyInstance i = (wrapValue i $
                     wrapConst nullPtr id) vkDestroyInstance

instanceExtensionProperties :: IO [Extension]
instanceExtensionProperties = wrapCountArray $ vkEnumerateInstanceExtensionProperties nullPtr

extensionProperties :: PhysicalDevice -> IO [Extension]
extensionProperties p =
  wrapValue p (wrapConst nullPtr wrapCountArray)
  vkEnumerateDeviceExtensionProperties

physicalDevices :: Instance -> IO [PhysicalDevice]
physicalDevices i = wrapValue i wrapCountArray vkEnumeratePhysicalDevices

instance FromVk Extent2D VkExtent2D where fromVk (VkExtent2D w h) = Extent2D <$> fromVk w <*> fromVk h
instance WithVk Extent2D VkExtent2D where withVk (Extent2D w h) fn = (wrapValue w $ wrapValue h fn) VkExtent2D

instance FromVk Extent3D VkExtent3D where fromVk (VkExtent3D w h d) = Extent3D <$> fromVk w <*> fromVk h <*> fromVk d
instance WithVk Extent3D VkExtent3D where withVk (Extent3D w h d) fn = (wrapValue w $ wrapValue h $ wrapValue d fn) VkExtent3D

ignored :: Word
ignored = fromIntegral VK_QUEUE_FAMILY_IGNORED

instance FromVk QueueFamilyProperties VkQueueFamilyProperties where
  fromVk (VkQueueFamilyProperties fl c tvb mitg) =
    QueueFamilyProperties <$> fromVk fl <*> fromVk c <*> fromVk tvb <*> fromVk mitg

instance WithVk QueueFamilyProperties VkQueueFamilyProperties where
  withVk (QueueFamilyProperties fl c tvb mitg) f =
    (wrapValue fl $ wrapValue c $ wrapValue tvb $ wrapValue mitg f) VkQueueFamilyProperties

queueFamilyProperties :: PhysicalDevice -> IO [QueueFamilyProperties]
queueFamilyProperties pd = wrapValue pd wrapCountArray vkGetPhysicalDeviceQueueFamilyProperties

createSurface :: Window -> Instance -> IO Surface
createSurface (Window w) (Handle i) = Handle <$>
  alloca (\ps -> do
             r <- createSurfaceFFI w i ps
             unless r $ error "SDL_CreateVulkanSurface failed"
             peek ps)

queueFamilySupportsPresent :: PhysicalDevice -> Word -> Surface -> IO Bool
queueFamilySupportsPresent pd qi s =
  (wrapValue pd $ wrapValue qi $ wrapValue s $ wrapOutPtr id)
  vkGetPhysicalDeviceSurfaceSupportKHR

instance WithVk QueueCreateInfo VkDeviceQueueCreateInfo where
  withVk (QueueCreateInfo i c) f =
    wrapInArray c
    f $ VkDeviceQueueCreateInfo VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO nullPtr
    (VkDeviceQueueCreateFlags zeroBits) (fromIntegral i)

instance WithVk DeviceCreateInfo VkDeviceCreateInfo where
  withVk (DeviceCreateInfo q l e) f =
    (wrapInArray q $
     wrapInArray l $
     wrapInArray e $
     f . ($ nullPtr))
    (VkDeviceCreateInfo VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO nullPtr (VkDeviceCreateFlags zeroBits))

createDevice :: PhysicalDevice -> DeviceCreateInfo -> IO Device
createDevice pd a =
  (wrapValue pd $ wrapInPtr a $ wrapConst nullPtr $ wrapOutPtr id)
  vkCreateDevice

getQueue :: Device -> Word -> Word -> IO Queue
getQueue d f i =
  (wrapValue d $ wrapValue f $ wrapValue i $ wrapOutPtr id)
  vkGetDeviceQueue

instance WithVk CommandPoolCreateInfo VkCommandPoolCreateInfo where
  withVk (CommandPoolCreateInfo (Flags fl) qfi) f =
    f $ VkCommandPoolCreateInfo VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO nullPtr
    fl (fromIntegral qfi)

createCommandPool :: Device -> CommandPoolCreateInfo -> IO CommandPool
createCommandPool d ci =
  (wrapValue d $ wrapInPtr ci $ wrapConst nullPtr $ wrapOutPtr id)
  vkCreateCommandPool

instance FromVk SurfaceFormat VkSurfaceFormatKHR where
  fromVk (VkSurfaceFormatKHR f cs) = pure (SurfaceFormat (Enumerator f) (Enumerator cs))

instance WithVk SurfaceFormat VkSurfaceFormatKHR where
  withVk (SurfaceFormat (Enumerator f) (Enumerator cs)) fn = fn (VkSurfaceFormatKHR f cs)

surfaceFormats :: PhysicalDevice -> Surface -> IO [SurfaceFormat]
surfaceFormats pd s =
  (wrapValue pd $ wrapValue s wrapCountArray)
  vkGetPhysicalDeviceSurfaceFormatsKHR

instance FromVk SurfaceCapabilities VkSurfaceCapabilitiesKHR where
  fromVk (VkSurfaceCapabilitiesKHR miic maic ce miie maie maial st ct sca suf) =
    SurfaceCapabilities <$> fromVk miic <*> fromVk maic <*> fromVk ce <*> fromVk miie
    <*> fromVk maie <*> fromVk maial <*> fromVk st <*> fromVk ct <*> fromVk sca <*> fromVk suf

instance WithVk SurfaceCapabilities VkSurfaceCapabilitiesKHR where
  withVk (SurfaceCapabilities miic maic ce miie maie maial st ct sca suf) f =
    (wrapValue miic $ wrapValue maic $ wrapValue ce $ wrapValue miie $ wrapValue maie $ wrapValue maial $
     wrapValue st $ wrapValue ct $ wrapValue sca $ wrapValue suf f) VkSurfaceCapabilitiesKHR

surfaceCapabilities :: PhysicalDevice -> Surface -> IO SurfaceCapabilities
surfaceCapabilities pd s =
  (wrapValue pd $ wrapValue s $ wrapOutPtr id)
  vkGetPhysicalDeviceSurfaceCapabilitiesKHR

allocateCommandBuffers :: Device -> CommandPool -> CommandBufferLevel -> Word -> IO [CommandBuffer]
allocateCommandBuffers d (Handle cp) (Enumerator l) n =
  (wrapValue d $
   wrapInPtr (VkCommandBufferAllocateInfo VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO nullPtr cp l (fromIntegral n)) $
   wrapOutArray n id)
  vkAllocateCommandBuffers

allocateCommandBuffer :: Device -> CommandPool -> CommandBufferLevel -> IO CommandBuffer
allocateCommandBuffer d cp l = head <$> allocateCommandBuffers d cp l 1

instance WithVk SwapchainCreateInfo VkSwapchainCreateInfoKHR where
  withVk (SwapchainCreateInfo f s mic (SurfaceFormat imf imcs) ie ial iu ism qfi pt ca pm c) fn =
    (wrapValue f $
     wrapValue s $
     wrapValue mic $
     wrapValue imf $
     wrapValue imcs $
     wrapValue ie $
     wrapValue ial $
     wrapValue iu $
     wrapValue ism $
     wrapInArray qfi $
     wrapValue pt $
     wrapValue ca $
     wrapValue pm $
     wrapValue c $
     wrapConst (VkSwapchainKHR 0)
     fn)
    (VkSwapchainCreateInfoKHR VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR nullPtr)

createSwapchain :: Device -> SwapchainCreateInfo -> IO Swapchain
createSwapchain d ci =
  (wrapValue d $ wrapInPtr ci $ wrapConst nullPtr $ wrapOutPtr id)
  vkCreateSwapchainKHR

swapchainImages :: Device -> Swapchain -> IO [Image]
swapchainImages d s =
  (wrapValue d $ wrapValue s wrapCountArray)
  vkGetSwapchainImagesKHR

foreign import ccall "wrapper" mkDebugReportCallback :: FNDebugReportCallback -> IO PFN_vkDebugReportCallbackEXT

createDebugReportCallback :: Instance -> DebugReportFlags -> DebugReportCallbackFun -> IO DebugReportCallback
createDebugReportCallback i (Flags flags) cb = do
  cbPtr <- mkDebugReportCallback wrappedCallback
  let ci = VkDebugReportCallbackCreateInfoEXT VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT nullPtr flags cbPtr nullPtr
  drc <- (wrapValue i $
          wrapInPtr (ci :: VkDebugReportCallbackCreateInfoEXT) $
          wrapConst nullPtr $
          wrapOutPtr id)
    vkCreateDebugReportCallbackEXT
  pure $ DebugReportCallback drc cbPtr
  where
    wrappedCallback :: FNDebugReportCallback
    wrappedCallback f ot o l mc lp m _ = do
      r <- join $ cb f ot o l mc <$> peekCString lp <*> peekCString m
      pure $ VkBool32 $ if r then 1 else 0

instance FromVk ComponentMapping VkComponentMapping where
  fromVk (VkComponentMapping r g b a) =
    pure (ComponentMapping (Enumerator r) (Enumerator g) (Enumerator b) (Enumerator a))

instance WithVk ComponentMapping VkComponentMapping where
  withVk (ComponentMapping (Enumerator r) (Enumerator g) (Enumerator b) (Enumerator a)) fn =
    fn (VkComponentMapping r g b a)

instance FromVk ImageSubresourceRange VkImageSubresourceRange where
  fromVk (VkImageSubresourceRange am bml lec bal lac) =
    ImageSubresourceRange <$> fromVk am <*> fromVk bml <*> fromVk lec <*> fromVk bal <*> fromVk lac
instance WithVk ImageSubresourceRange VkImageSubresourceRange where
  withVk (ImageSubresourceRange am bml lec bal lac) fn =
    (wrapValue am $ wrapValue bml $ wrapValue lec $ wrapValue bal $ wrapValue lac fn) VkImageSubresourceRange

instance WithVk ImageCreateInfo VkImageCreateInfo where
  withVk (ImageCreateInfo f it fo e ml al s t u sm qfi il) fn =
    (wrapValue f $
     wrapValue it $
     wrapValue fo $
     wrapValue e $
     wrapValue ml $
     wrapValue al $
     wrapValue s $
     wrapValue t $
     wrapValue u $
     wrapValue sm $
     wrapInArray qfi $
     wrapValue il  fn)
    (VkImageCreateInfo VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO nullPtr)

createImage :: Device -> ImageCreateInfo -> IO Image
createImage d ci =
  (wrapValue d $ wrapInPtr ci $ wrapConst nullPtr $ wrapOutPtr id)
  vkCreateImage

instance WithVk ImageViewCreateInfo VkImageViewCreateInfo where
  withVk (ImageViewCreateInfo f i vt fo c sr) fn =
    (wrapValue f $ wrapValue i $ wrapValue vt $ wrapValue fo $ wrapValue c $ wrapValue sr fn)
    (VkImageViewCreateInfo VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO nullPtr)

createImageView :: Device -> ImageViewCreateInfo -> IO ImageView
createImageView d ci =
  (wrapValue d $ wrapInPtr ci $ wrapConst nullPtr $ wrapOutPtr id)
  vkCreateImageView

instance FromVk AttachmentDescription VkAttachmentDescription where
  fromVk (VkAttachmentDescription f fo s lo so slo sso il fl) =
    pure (AttachmentDescription (Flags f) (Enumerator fo) (Flags s) (Enumerator lo)
          (Enumerator so) (Enumerator slo) (Enumerator sso) (Enumerator il) (Enumerator fl))
instance WithVk AttachmentDescription VkAttachmentDescription where
  withVk (AttachmentDescription (Flags f) (Enumerator fo) (Flags s) (Enumerator lo)
          (Enumerator so) (Enumerator slo) (Enumerator sso) (Enumerator il) (Enumerator fl)) fn =
    fn (VkAttachmentDescription f fo s lo so slo sso il fl)

instance WithVk AttachmentReference VkAttachmentReference where
  withVk (AttachmentReference a l) fn =
    (wrapValue a $ wrapValue l fn) VkAttachmentReference

instance WithVk SubpassDescription VkSubpassDescription where
  withVk (SubpassDescription f pbp ia ca pa) fn =
    (wrapValue f $ wrapValue pbp $ wrapInArray ia $ wrapInArray ca $ wrapConst nullPtr $ wrapConst nullPtr $ wrapInArray pa fn)
    VkSubpassDescription

instance WithVk RenderPassCreateInfo VkRenderPassCreateInfo where
  withVk (RenderPassCreateInfo f a s d) fn =
    (wrapValue f $ wrapInArray a $ wrapInArray s $ wrapInArray d fn)
    (VkRenderPassCreateInfo VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO nullPtr)

createRenderPass :: Device -> RenderPassCreateInfo -> IO RenderPass
createRenderPass d ci =
  (wrapValue d $ wrapInPtr ci $ wrapConst nullPtr $ wrapOutPtr id) vkCreateRenderPass

instance WithVk FramebufferCreateInfo VkFramebufferCreateInfo where
  withVk (FramebufferCreateInfo f rp a w h l) fn =
    (wrapValue f $ wrapValue rp $ wrapInArray a $ wrapValue w $ wrapValue h $ wrapValue l fn)
    (VkFramebufferCreateInfo VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO nullPtr)

createFramebuffer :: Device -> FramebufferCreateInfo -> IO Framebuffer
createFramebuffer d ci =
  (wrapValue d $ wrapInPtr ci $ wrapConst nullPtr $ wrapOutPtr id)
  vkCreateFramebuffer

createSemaphore :: Device -> IO Semaphore
createSemaphore d =
  (wrapValue d $
   wrapInPtr (VkSemaphoreCreateInfo VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO nullPtr zeroBits) $
   wrapConst nullPtr $
   wrapOutPtr id)
  vkCreateSemaphore

destroySemaphore :: Device -> Semaphore -> IO ()
destroySemaphore d s =
  (wrapValue d $ wrapValue s $ wrapConst nullPtr id)
  vkDestroySemaphore

acquireNextImage :: Device -> Swapchain -> Semaphore -> IO Word
acquireNextImage d sc s =
  (wrapValue d $ wrapValue sc $ wrapConst maxBound $ wrapValue s $ wrapValue nullHandle $ wrapOutPtr id)
  vkAcquireNextImageKHR

beginCommandBuffer :: CommandBuffer -> IO ()
beginCommandBuffer cb =
  (wrapValue cb $ 
   wrapInPtr (VkCommandBufferBeginInfo VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO nullPtr zeroBits nullPtr) id)
  vkBeginCommandBuffer >>= check

endCommandBuffer :: CommandBuffer -> IO ()
endCommandBuffer cb = wrapValue cb id vkEndCommandBuffer >>= check

instance WithVk MemoryBarrier VkMemoryBarrier where
  withVk (MemoryBarrier (Flags sam) (Flags dam)) f =
    f (VkMemoryBarrier VK_STRUCTURE_TYPE_MEMORY_BARRIER nullPtr sam dam)

instance FromVk DeviceSize VkDeviceSize where
  fromVk (VkDeviceSize s) = pure $ DeviceSize s

instance WithVk DeviceSize VkDeviceSize where
  withVk (DeviceSize s) fn = fn $ VkDeviceSize s

instance WithVk BufferMemoryBarrier VkBufferMemoryBarrier where
  withVk (BufferMemoryBarrier sam dam sqfi dqfi b o s) f =
    (wrapValue sam $ wrapValue dam $ wrapValue sqfi $ wrapValue dqfi $ wrapValue b $ wrapValue o $ wrapValue s f)
    (VkBufferMemoryBarrier VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER nullPtr)

instance WithVk ImageMemoryBarrier VkImageMemoryBarrier where
  withVk (ImageMemoryBarrier sam dam ol nl sqfi dqfi i sr) f =
    (wrapValue sam $ wrapValue dam $ wrapValue ol $ wrapValue nl $ wrapValue sqfi $ wrapValue dqfi $ wrapValue i $ wrapValue sr f)
    (VkImageMemoryBarrier VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER nullPtr)

cmdPipelineBarrier :: CommandBuffer -> PipelineStage -> PipelineStage -> DependencyFlags ->
                      [MemoryBarrier] -> [BufferMemoryBarrier] -> [ImageMemoryBarrier] -> IO ()
cmdPipelineBarrier cb ssm dsm df mb bmb imb =
  (wrapValue cb $ wrapValue ssm $ wrapValue dsm $ wrapValue df $ wrapInArray mb $ wrapInArray bmb $ wrapInArray imb id)
  vkCmdPipelineBarrier

instance FromVk Offset2D VkOffset2D where fromVk (VkOffset2D x y) = pure (Offset2D x y)
instance WithVk Offset2D VkOffset2D where withVk (Offset2D x y) fn = fn (VkOffset2D x y)

instance WithVk Rect2D VkRect2D where withVk (Rect2D o e) fn = (wrapValue o $ wrapValue e fn) VkRect2D

instance WithVk ClearColorValue VkClearColorValue where
  withVk (FloatColor v) fn = fn $ VkFloat32 $ V.map CFloat v
  withVk (IntColor v) fn = fn $ VkInt32 $ V.map fromIntegral v
  withVk (UIntColor v) fn = fn $ VkUint32 $ V.map fromIntegral v

instance WithVk ClearDepthStencilValue VkClearDepthStencilValue where
  withVk (ClearDepthStencilValue d s) fn = (wrapValue d $ wrapValue s fn) VkClearDepthStencilValue

instance WithVk ClearValue VkClearValue where
  withVk (ClearColor v) fn = wrapValue v fn VkColor
  withVk (ClearDepthStencil v) fn = wrapValue v fn VkDepthStencil

instance WithVk RenderPassBeginInfo VkRenderPassBeginInfo where
  withVk (RenderPassBeginInfo rp fb ra cv) f =
    (wrapValue rp $ wrapValue fb $ wrapValue ra $ wrapInArray cv f)
    (VkRenderPassBeginInfo VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO nullPtr)

cmdBeginRenderPass :: CommandBuffer -> RenderPassBeginInfo -> SubpassContents -> IO ()
cmdBeginRenderPass cb bi sc =
  (wrapValue cb $ wrapInPtr bi $ wrapValue sc id)
  vkCmdBeginRenderPass

cmdEndRenderPass :: CommandBuffer -> IO ()
cmdEndRenderPass cb = wrapValue cb id vkCmdEndRenderPass >>= check

cmdBindPipeline :: CommandBuffer -> PipelineBindPoint -> Pipeline -> IO ()
cmdBindPipeline cb pbp p =
  (wrapValue cb $
   wrapValue pbp $
   wrapValue p id)
  vkCmdBindPipeline

cmdSetViewports :: CommandBuffer -> Word -> [Viewport] -> IO ()
cmdSetViewports cb i vp =
  (wrapValue cb $
   wrapValue i $
   wrapInArray vp id)
  vkCmdSetViewport

cmdSetViewport :: CommandBuffer -> Word -> Viewport -> IO ()
cmdSetViewport cb i s = cmdSetViewports cb i [s]

cmdSetScissors :: CommandBuffer -> Word -> [Rect2D] -> IO ()
cmdSetScissors cb i s =
  (wrapValue cb $
   wrapValue i $
   wrapInArray s id)
  vkCmdSetScissor

cmdSetScissor :: CommandBuffer -> Word -> Rect2D -> IO ()
cmdSetScissor cb i vb = cmdSetScissors cb i [vb]

cmdBindVertexBuffers :: CommandBuffer -> Word -> [(Buffer, DeviceSize)] -> IO ()
cmdBindVertexBuffers cb i vb =
  (wrapValue cb $
   wrapValue i $
   wrapInArray (fst <$> vb) $
   wrapInArrayNoCount (snd <$> vb) id)
  vkCmdBindVertexBuffers

cmdBindVertexBuffer :: CommandBuffer -> Word -> Buffer -> DeviceSize -> IO ()
cmdBindVertexBuffer cb i vb o = cmdBindVertexBuffers cb i [(vb, o)]

cmdBindDescriptorSets :: CommandBuffer -> PipelineBindPoint -> PipelineLayout -> Word -> [DescriptorSet] -> [Word] -> IO ()
cmdBindDescriptorSets cb pbp pl fs ds dyo =
  (wrapValue cb $
   wrapValue pbp $
   wrapValue pl $
   wrapValue fs $
   wrapInArray ds $
   wrapInArray dyo id)
  vkCmdBindDescriptorSets

cmdDraw :: CommandBuffer -> Word -> Word -> Word -> Word -> IO ()
cmdDraw cb vc ic fv fi =
  (wrapValue cb $
   wrapValue vc $
   wrapValue ic $
   wrapValue fv $
   wrapValue fi id)
  vkCmdDraw

instance WithVk SubmitInfo VkSubmitInfo where
  withVk (SubmitInfo ws cb ss) f =
    wrapInArray (fmap fst ws)
    (wrapInArrayNoCount (fmap snd ws) $
     wrapInArray cb $
     wrapInArray ss f) $
    VkSubmitInfo VK_STRUCTURE_TYPE_SUBMIT_INFO nullPtr

queueSubmit :: Queue -> [SubmitInfo] -> Fence -> IO ()
queueSubmit q si f = (wrapValue q $ wrapInArray si $ wrapValue f id) vkQueueSubmit >>= check

instance WithVk PresentInfo VkPresentInfoKHR where
  withVk (PresentInfo ws sc) f =
    wrapInArray ws
    (wrapInArray (fst <$> sc) $
     wrapInArrayNoCount (snd <$> sc) $
     wrapConst nullPtr f) $
    VkPresentInfoKHR VK_STRUCTURE_TYPE_PRESENT_INFO_KHR nullPtr

queuePresent :: Queue -> PresentInfo -> IO ()
queuePresent q pi = (wrapValue q $ wrapInPtr pi id) vkQueuePresentKHR >>= check

queueWaitIdle :: Queue -> IO ()
queueWaitIdle q = wrapValue q id vkQueueWaitIdle >>= check

instance WithVk SpecializationMapEntry VkSpecializationMapEntry where
  withVk (SpecializationMapEntry c o s) fn =
    (wrapValue c $ wrapValue o $ wrapConst s fn)
    VkSpecializationMapEntry

instance WithVk SpecializationInfo VkSpecializationInfo where
  withVk (SpecializationInfo me ds p) fn =
    (wrapInArray me $ wrapConst ds $ wrapConst p fn)
    VkSpecializationInfo

instance WithVk PipelineShaderStageCreateInfo VkPipelineShaderStageCreateInfo where
  withVk (PipelineShaderStageCreateInfo f s sm n si) fn =
    (wrapValue f $ wrapValue s $ wrapValue sm $ wrapValue n $ wrapOptPtr si fn)
    (VkPipelineShaderStageCreateInfo VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO nullPtr)

instance WithVk VertexInputBindingDescription VkVertexInputBindingDescription where
  withVk (VertexInputBindingDescription b s ir) fn =
    (wrapValue b $ wrapValue s $ wrapValue ir fn)
    VkVertexInputBindingDescription

instance WithVk VertexInputAttributeDescription VkVertexInputAttributeDescription where
  withVk (VertexInputAttributeDescription l b f o) fn =
    (wrapValue l $ wrapValue b $ wrapValue f $ wrapValue o fn)
    VkVertexInputAttributeDescription

instance WithVk PipelineVertexInputStateCreateInfo VkPipelineVertexInputStateCreateInfo where
  withVk (PipelineVertexInputStateCreateInfo f vbd vad) fn =
    (wrapValue f $ wrapInArray vbd $ wrapInArray vad fn)
    (VkPipelineVertexInputStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO nullPtr)

instance WithVk PipelineInputAssemblyStateCreateInfo VkPipelineInputAssemblyStateCreateInfo where
  withVk (PipelineInputAssemblyStateCreateInfo f t p) fn =
    (wrapValue f $ wrapValue t $ wrapValue p fn)
    (VkPipelineInputAssemblyStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO nullPtr)

instance WithVk PipelineTessellationStateCreateInfo VkPipelineTessellationStateCreateInfo where
  withVk (PipelineTessellationStateCreateInfo f p) fn =
    (wrapValue f $ wrapValue p fn)
    (VkPipelineTessellationStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO nullPtr)

instance WithVk Viewport VkViewport where
  withVk (Viewport x y w h mi ma) fn =
    (wrapValue x $
     wrapValue y $
     wrapValue w $
     wrapValue h $
     wrapValue mi $
     wrapValue ma fn)
    VkViewport

instance WithVk PipelineViewportStateCreateInfo VkPipelineViewportStateCreateInfo where
  withVk (PipelineViewportStateCreateInfo f vc v sc s) fn =
    (wrapValue f $ wrapValue vc $ wrapOptArrayNoCount v $ wrapValue sc $ wrapOptArrayNoCount s fn)
    (VkPipelineViewportStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO nullPtr)

instance WithVk PipelineRasterizationStateCreateInfo VkPipelineRasterizationStateCreateInfo where
  withVk (PipelineRasterizationStateCreateInfo f dce rde pm cm ff dbe dbcf dbc dbsf lw) fn =
    (wrapValue f $
     wrapValue dce $
     wrapValue rde $
     wrapValue pm $
     wrapValue cm $
     wrapValue ff $
     wrapValue dbe $
     wrapValue dbcf $
     wrapValue dbc $
     wrapValue dbsf $
     wrapValue lw fn)
    (VkPipelineRasterizationStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO nullPtr)

instance WithVk SampleMask VkSampleMask where
  withVk (SampleMask m) fn =
    wrapValue m fn
    VkSampleMask

instance WithVk PipelineMultisampleStateCreateInfo VkPipelineMultisampleStateCreateInfo where
  withVk (PipelineMultisampleStateCreateInfo f rs sse mss sm atce atoe) fn =
    (wrapValue f $
     wrapValue rs $
     wrapValue sse $
     wrapValue mss $
     wrapOptArrayNoCount sm $
     wrapValue atce $
     wrapValue atoe fn)
    (VkPipelineMultisampleStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO nullPtr)

instance WithVk StencilOpState VkStencilOpState where
  withVk (StencilOpState fo po dfo co cm wm r) fn =
    (wrapValue fo $
     wrapValue po $
     wrapValue dfo $
     wrapValue co $
     wrapValue cm $
     wrapValue wm $
     wrapValue r fn)
    VkStencilOpState

instance WithVk PipelineDepthStencilStateCreateInfo VkPipelineDepthStencilStateCreateInfo where
  withVk (PipelineDepthStencilStateCreateInfo fl dte dwe dco dbte ste f b midb madb) fn =
    (wrapValue fl $
     wrapValue dte $
     wrapValue dwe $
     wrapValue dco $
     wrapValue dbte $
     wrapValue ste $
     wrapValue f $
     wrapValue b $
     wrapValue midb $
     wrapValue madb fn)
    (VkPipelineDepthStencilStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO nullPtr)

all :: ColorComponent
all = Red .|. Green .|. Blue .|. Alpha

instance WithVk PipelineColorBlendAttachmentState VkPipelineColorBlendAttachmentState where
  withVk (PipelineColorBlendAttachmentState be scbf dcbf cbo sabf dabf abo cwm) fn =
    (wrapValue be $
     wrapValue scbf $
     wrapValue dcbf $
     wrapValue cbo $
     wrapValue sabf $
     wrapValue dabf $
     wrapValue abo $
     wrapValue cwm fn)
    VkPipelineColorBlendAttachmentState

instance WithVk PipelineColorBlendStateCreateInfo VkPipelineColorBlendStateCreateInfo where
  withVk (PipelineColorBlendStateCreateInfo f loe lo a bc) fn =
    (wrapValue f $
     wrapValue loe $
     wrapValue lo $
     wrapInArray a $
     wrapConst bc fn)
    (VkPipelineColorBlendStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO nullPtr)

instance WithVk PipelineDynamicStateCreateInfo VkPipelineDynamicStateCreateInfo where
  withVk (PipelineDynamicStateCreateInfo f ds) fn =
    (wrapValue f $
     wrapInArray ds fn)
    (VkPipelineDynamicStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO nullPtr)

instance WithVk GraphicsPipelineCreateInfo VkGraphicsPipelineCreateInfo where
  withVk (GraphicsPipelineCreateInfo f s vis ias ts vs rs ms dss cbs ds l rp sp bph bpi) fn =
    (wrapValue f $
     wrapInArray s $
     wrapInPtr vis $
     wrapInPtr ias $
     wrapOptPtr ts $
     wrapOptPtr vs $
     wrapInPtr rs $
     wrapOptPtr ms $
     wrapInPtr dss $
     wrapInPtr cbs $
     wrapOptPtr ds $
     wrapValue l $
     wrapValue rp $
     wrapValue sp $
     wrapValue bph $
     wrapValue bpi fn)
    (VkGraphicsPipelineCreateInfo VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO nullPtr)

createGraphicsPipelines :: Device -> [GraphicsPipelineCreateInfo] -> IO [Pipeline]
createGraphicsPipelines d ci =
  (wrapValue d $
   wrapConst (VkPipelineCache 0) $
   wrapInArray ci $
   wrapConst nullPtr $
   wrapOutArray (fromIntegral $ length ci) id)
  vkCreateGraphicsPipelines

createGraphicsPipeline :: Device -> GraphicsPipelineCreateInfo -> IO Pipeline
createGraphicsPipeline d ci = head <$> createGraphicsPipelines d [ci]

instance WithVk DescriptorSetLayoutBinding VkDescriptorSetLayoutBinding where
  withVk (DescriptorSetLayoutBinding b dt dc sf is) fn =
    (wrapValue b $
     wrapValue dt $
     wrapValue dc $
     wrapValue sf $
     wrapOptArrayNoCount is fn)
    VkDescriptorSetLayoutBinding

instance WithVk DescriptorSetLayoutCreateInfo VkDescriptorSetLayoutCreateInfo where
  withVk (DescriptorSetLayoutCreateInfo f b) fn =
    (wrapValue f $
     wrapInArray b fn)
    (VkDescriptorSetLayoutCreateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO nullPtr)

createDescriptorSetLayout :: Device -> DescriptorSetLayoutCreateInfo -> IO DescriptorSetLayout
createDescriptorSetLayout d ci =
  (wrapValue d $
   wrapInPtr ci $
   wrapConst nullPtr $
   wrapOutPtr id)
  vkCreateDescriptorSetLayout

instance WithVk PushConstantRange VkPushConstantRange where
  withVk (PushConstantRange sf o s) fn =
    (wrapValue sf $
     wrapValue o $
     wrapValue s fn)
    VkPushConstantRange

instance WithVk PipelineLayoutCreateInfo VkPipelineLayoutCreateInfo where
  withVk (PipelineLayoutCreateInfo f sl pcr) fn =
    (wrapValue f $
     wrapInArray sl $
     wrapInArray pcr fn)
    (VkPipelineLayoutCreateInfo VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO nullPtr)

createPipelineLayout :: Device -> PipelineLayoutCreateInfo -> IO PipelineLayout
createPipelineLayout d ci =
  (wrapValue d $
   wrapInPtr ci $
   wrapConst nullPtr $
   wrapOutPtr id)
  vkCreatePipelineLayout

instance WithVk ShaderModuleCreateInfo VkShaderModuleCreateInfo where
  withVk (ShaderModuleCreateInfo f c) fn =
    (wrapValue f $
     wrapByteString c fn)
    (VkShaderModuleCreateInfo VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO nullPtr)

createShaderModule :: Device -> ShaderModuleCreateInfo -> IO ShaderModule
createShaderModule d ci =
  (wrapValue d $
   wrapInPtr ci $
   wrapConst nullPtr $
   wrapOutPtr id)
  vkCreateShaderModule

instance WithVk BufferCreateInfo VkBufferCreateInfo where
  withVk (BufferCreateInfo f s u sm qfi) fn =
    (wrapValue f $
     wrapValue s $
     wrapValue u $
     wrapValue sm $
     wrapInArray qfi fn)
    (VkBufferCreateInfo VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO nullPtr)

createBuffer :: Device -> BufferCreateInfo -> IO Buffer
createBuffer d ci =
  (wrapValue d $
   wrapInPtr ci $
   wrapConst nullPtr $
   wrapOutPtr id)
  vkCreateBuffer

instance FromVk MemoryRequirements VkMemoryRequirements where
  fromVk (VkMemoryRequirements s a mtb) = MemoryRequirements <$> fromVk s <*> fromVk a <*> fromVk mtb

class HasMemoryRequirements a where memoryRequirements :: Device -> a -> IO MemoryRequirements

instance HasMemoryRequirements Buffer where
  memoryRequirements d b =
    (wrapValue d $
     wrapValue b $
     wrapOutPtr id)
    vkGetBufferMemoryRequirements

instance HasMemoryRequirements Image where
  memoryRequirements d b =
    (wrapValue d $
     wrapValue b $
     wrapOutPtr id)
    vkGetImageMemoryRequirements

instance FromVk MemoryType VkMemoryType where
  fromVk (VkMemoryType pf hi) = MemoryType <$> fromVk pf <*> fromVk hi

instance FromVk MemoryHeap VkMemoryHeap where
  fromVk (VkMemoryHeap s f) = MemoryHeap <$> fromVk s <*> fromVk f

instance FromVk PhysicalDeviceMemoryProperties VkPhysicalDeviceMemoryProperties where
  fromVk (VkPhysicalDeviceMemoryProperties mtc mt mhc mh) =
    PhysicalDeviceMemoryProperties <$> fromLenVector mtc mt <*> fromLenVector mhc mh

memoryProperties :: PhysicalDevice -> IO PhysicalDeviceMemoryProperties
memoryProperties pd =
  (wrapValue pd $
   wrapOutPtr id)
  vkGetPhysicalDeviceMemoryProperties

instance WithVk MemoryAllocateInfo VkMemoryAllocateInfo where
  withVk (MemoryAllocateInfo as mti) fn =
    (wrapValue as $
     wrapValue mti fn)
    (VkMemoryAllocateInfo VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO nullPtr)

allocate :: Device -> MemoryAllocateInfo -> IO DeviceMemory
allocate d mai =
  (wrapValue d $
   wrapInPtr mai $
   wrapConst nullPtr $
   wrapOutPtr id)
  vkAllocateMemory

mapMemory :: Device -> DeviceMemory -> DeviceSize -> DeviceSize -> MemoryMapFlags -> IO (Ptr Void)
mapMemory d dm o s f =
  (wrapValue d $
   wrapValue dm $
   wrapValue o $
   wrapValue s $
   wrapValue f $
   wrapOutPtr id)
  vkMapMemory

unmapMemory :: Device -> DeviceMemory -> IO ()
unmapMemory d dm =
  (wrapValue d $
   wrapValue dm id)
  vkUnmapMemory

class BindMemory a where bindMemory :: Device -> a -> DeviceMemory -> DeviceSize -> IO ()

instance BindMemory Buffer where
  bindMemory d b dm o =
    (wrapValue d $
     wrapValue b $
     wrapValue dm $
     wrapValue o id)
    vkBindBufferMemory >>= check

instance BindMemory Image where
  bindMemory d b dm o =
    (wrapValue d $
     wrapValue b $
     wrapValue dm $
     wrapValue o id)
    vkBindImageMemory >>= check

instance WithVk DescriptorPoolSize VkDescriptorPoolSize where
  withVk (DescriptorPoolSize dt dc) f =
    (wrapValue dt $
     wrapValue dc f)
    VkDescriptorPoolSize

instance WithVk DescriptorPoolCreateInfo VkDescriptorPoolCreateInfo where
  withVk (DescriptorPoolCreateInfo f ms ps) fn =
    (wrapValue f $
     wrapValue ms $
     wrapInArray ps
     fn)
    (VkDescriptorPoolCreateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO nullPtr)

createDescriptorPool :: Device -> DescriptorPoolCreateInfo -> IO DescriptorPool
createDescriptorPool d ci =
  (wrapValue d $
   wrapInPtr ci $
   wrapConst nullPtr $
   wrapOutPtr id)
  vkCreateDescriptorPool

instance WithVk DescriptorSetAllocateInfo VkDescriptorSetAllocateInfo where
  withVk (DescriptorSetAllocateInfo dp sl) fn =
    (wrapValue dp $
     wrapInArray sl fn)
    (VkDescriptorSetAllocateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO nullPtr)

allocateDescriptorSets :: Device -> DescriptorSetAllocateInfo -> IO [DescriptorSet]
allocateDescriptorSets d ci =
  (wrapValue d $
   wrapInPtr ci $
   wrapOutArray (fromIntegral $ length $ setLayouts (ci :: DescriptorSetAllocateInfo)) id)
  vkAllocateDescriptorSets

allocateDescriptorSet :: Device -> DescriptorPool -> DescriptorSetLayout -> IO DescriptorSet
allocateDescriptorSet d dp dsl = head <$> allocateDescriptorSets d (DescriptorSetAllocateInfo dp [dsl])

instance WithVk DescriptorImageInfo VkDescriptorImageInfo where
  withVk (DescriptorImageInfo s iv il) fn =
    (wrapValue s $
     wrapValue iv $
     wrapValue il fn)
    VkDescriptorImageInfo

instance WithVk DescriptorBufferInfo VkDescriptorBufferInfo where
  withVk (DescriptorBufferInfo b o r) fn =
    (wrapValue b $
     wrapValue o $
     wrapValue r fn)
    VkDescriptorBufferInfo

instance WithVk WriteDescriptorSet VkWriteDescriptorSet where
  withVk (WriteDescriptorSet ds db dae dc dt ii bi tbv) fn =
    (wrapValue ds $
     wrapValue db $
     wrapValue dae $
     wrapValue dc $
     wrapValue dt $
     wrapOptArrayNoCount ii $
     wrapOptArrayNoCount bi $
     wrapOptArrayNoCount tbv fn)
    (VkWriteDescriptorSet VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET nullPtr)

instance WithVk CopyDescriptorSet VkCopyDescriptorSet where
  withVk (CopyDescriptorSet ss sb sae ds db dae dc) fn =
    (wrapValue ss $
     wrapValue sb $
     wrapValue sae $
     wrapValue ds $
     wrapValue db $
     wrapValue dae $
     wrapValue dc fn)
    (VkCopyDescriptorSet VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET nullPtr)

updateDescriptorSets :: Device -> [WriteDescriptorSet] -> [CopyDescriptorSet] -> IO ()
updateDescriptorSets d wds cds =
  (wrapValue d $
   wrapInArray wds $
   wrapInArray cds id)
  vkUpdateDescriptorSets

instance WithVk SamplerCreateInfo VkSamplerCreateInfo where
  withVk (SamplerCreateInfo f maf mif mm amu amv amw mlb ae ma ce co mil mal bc uc) fn =
    (wrapValue f $
     wrapValue maf $
     wrapValue mif $
     wrapValue mm $
     wrapValue amu $
     wrapValue amv $
     wrapValue amw $
     wrapValue mlb $
     wrapValue ae $
     wrapValue ma $
     wrapValue ce $
     wrapValue co $
     wrapValue mil $
     wrapValue mal $
     wrapValue bc $
     wrapValue uc fn)
    (VkSamplerCreateInfo VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO nullPtr)

createSampler :: Device -> SamplerCreateInfo -> IO Sampler
createSampler d ci =
  (wrapValue d $
   wrapInPtr ci $
   wrapConst nullPtr $
   wrapOutPtr id)
  vkCreateSampler
