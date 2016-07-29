{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Main where

import Prelude hiding (readFile)

import Control.Exception
import Control.Monad
import Data.Bits
import Data.ByteString (readFile)
import Data.Vector.Storable.Sized as V (replicate)
import Graphics.Vulkan as Vk
import Linear.V2
import SDL hiding (Surface)
import SDL.Video.Vulkan
import System.Environment

import Paths_vulkan_hl

findAndCreateDevice :: Instance -> Surface -> IO (PhysicalDevice, Word, Device, Queue, CommandPool)
findAndCreateDevice inst surface = do
  m <- findDevice =<< physicalDevices inst
  case m of
    Just (pd, qf) -> do
      d <- createDevice pd $ DeviceCreateInfo [QueueCreateInfo qf [0]] ["VK_LAYER_LUNARG_standard_validation"] ["VK_KHR_swapchain"]
      q <- getQueue d qf 0
      cp <- createCommandPool d $ CommandPoolCreateInfo CreateResetCommandBuffer qf
      return (pd, qf, d, q, cp)
    Nothing -> error "No Device"
  where
    findDevice [] = return Nothing
    findDevice (d:ds) = do
      x <- findQueue 0 =<< queueFamilyProperties d
      case x of
        Just y -> return $ Just y
        Nothing -> findDevice ds
      where
        findQueue _ [] = return Nothing
        findQueue i (q:qs) = do
          s <- queueFamilySupportsPresent d i surface
          if flags (q :: QueueFamilyProperties) `hasFlags` GraphicsQueue && s
            then return $ Just (d, i)
            else findQueue (succ i) qs

findSurfaceFormat :: [SurfaceFormat] -> SurfaceFormat
findSurfaceFormat [SurfaceFormat UndefinedFormat cs] = SurfaceFormat B8G8R8A8Srgb cs
findSurfaceFormat (sf:_) = sf
findSurfaceFormat [] = error "no surface format"

swapchainExtents :: Window -> SurfaceCapabilities -> IO Extent2D
swapchainExtents w sc =
  case currentExtent sc of
    Extent2D (-1) _ -> (\(V2 x y) -> Extent2D (fromIntegral x) (fromIntegral y)) <$> get (windowSize w)
    p -> return p

swapchainImageCount :: SurfaceCapabilities -> Word
swapchainImageCount sc =
  let ic = minImageCount (sc :: SurfaceCapabilities) + 1
  in fromIntegral $ case maxImageCount sc of
    0 -> ic
    mic -> min ic mic

run :: Window -> IO ()
run window = do
  setEnv "VK_LOADER_DEBUG" "all"
  iext <- requiredInstanceExtensions
  let
    app = ApplicationInfo "vulkan-test" (Version 0 0 0) "none" (Version 0 0 0) (Version 1 0 3)
    ici = InstanceCreateInfo app ["VK_LAYER_LUNARG_standard_validation"] ("VK_EXT_debug_report" : "VK_KHR_surface" : iext)
  inst <- createInstance ici
  cb <- createDebugReportCallback inst
    (Vk.Error .|.
     Vk.Warning .|.
     PerformanceWarning .|.
     Vk.Debug)
    (\f ot o l mc lp m -> print (f, ot, o, l, mc, lp, m) >> return False)
  surface <- createSurface window inst
  (physicalDevice, qf, device, queue, commandPool) <- findAndCreateDevice inst surface
  surfaceFormat <- findSurfaceFormat <$> surfaceFormats physicalDevice surface
  surfaceCaps <- surfaceCapabilities physicalDevice surface
  commandBuffer <- allocateCommandBuffer device commandPool Primary
  extent@(Extent2D width height) <- swapchainExtents window surfaceCaps
  let imageCount = swapchainImageCount surfaceCaps
  swapchain <- createSwapchain device (SwapchainCreateInfo zeroBits surface imageCount surfaceFormat
                                       extent 1 ColorAttachment Exclusive
                                       [qf] (currentTransform surfaceCaps) Opaque
                                       Fifo True)
  images <- swapchainImages device swapchain
  imageViews <- mapM
    (\image -> createImageView device $ ImageViewCreateInfo zeroBits image Type2D
               (format (surfaceFormat :: SurfaceFormat))
               (ComponentMapping R G B A)
               (ImageSubresourceRange Color 0 1 0 1)) images
  let attachment = AttachmentDescription zeroBits
        (format (surfaceFormat :: SurfaceFormat)) Sample1 Clear Store
        DontCareLoad DontCareStore ColorAttachmentOptimal
        ColorAttachmentOptimal
      subpass = SubpassDescription zeroBits GraphicsBindPoint
        [] [AttachmentReference 0 ColorAttachmentOptimal] []
  renderPass <- createRenderPass device (RenderPassCreateInfo zeroBits [attachment] [subpass] [])
  framebuffers <- mapM
    (\iv -> createFramebuffer device $ FramebufferCreateInfo zeroBits renderPass [iv] width height 1) imageViews
  print ( device
        , queue
        , commandPool
        , surfaceFormat
        , commandBuffer
        , surfaceCaps
        , extent
        , imageCount
        , swapchain
        , images
        , cb
        , imageViews
        , renderPass
        , framebuffers
        )

  dsLayout <- createDescriptorSetLayout device $ DescriptorSetLayoutCreateInfo zeroBits
    [DescriptorSetLayoutBinding 0 CombinedImageSampler 1 Fragment Nothing]

  pipelineLayout <- createPipelineLayout device $ PipelineLayoutCreateInfo zeroBits [dsLayout] []

  memProp <- memoryProperties physicalDevice

  buffer <- createBuffer device $ BufferCreateInfo zeroBits (3 * 5 * 4) VertexBuffer Exclusive []
  memReq <- memoryRequirements device buffer

  let memTypeIndex = findMemType 0 (memoryType memProp) memReq HostVisible
      findMemType :: Word -> [MemoryType] -> MemoryRequirements -> MemoryProperty -> Word
      findMemType i ((MemoryType p _):ms) (MemoryRequirements _ _ t) f | hasFlags p f && testBit t (fromIntegral i) = i
      findMemType i (m:ms) mr f = findMemType (succ i) ms mr f

  mem <- allocate device $ MemoryAllocateInfo (size (memReq :: MemoryRequirements)) memTypeIndex

  vertexShader <- getDataFileName "tri-vert.spv" >>= readFile
  fragmentShader <- getDataFileName "tri-frag.spv" >>= readFile
  vertexShaderModule <- createShaderModule device $ ShaderModuleCreateInfo zeroBits vertexShader
  fragmentShaderModule <- createShaderModule device $ ShaderModuleCreateInfo zeroBits fragmentShader

  pipeline <- createGraphicsPipeline device $
    GraphicsPipelineCreateInfo
    zeroBits
    [ PipelineShaderStageCreateInfo zeroBits Vertex vertexShaderModule "main" Nothing
    , PipelineShaderStageCreateInfo zeroBits Fragment fragmentShaderModule "main" Nothing
    ]
    (PipelineVertexInputStateCreateInfo zeroBits
      [VertexInputBindingDescription 0 (3 * 5 * 4) PerVertex]
      [ VertexInputAttributeDescription 0 0 R32G32B32SFloat 0
      , VertexInputAttributeDescription 1 0 R32G32SFloat (3 * 4)
      ])
    (PipelineInputAssemblyStateCreateInfo zeroBits TriangleList False)
    Nothing
    (Just $ PipelineViewportStateCreateInfo zeroBits 1 Nothing 1 Nothing)
    (PipelineRasterizationStateCreateInfo
      zeroBits
      False
      False
      Fill
      Back
      Clockwise
      False 0 0 0
      1)
    (Just $ PipelineMultisampleStateCreateInfo zeroBits Sample1 False 0 Nothing False False)
    (PipelineDepthStencilStateCreateInfo zeroBits True True LessOrEqual False False
      (StencilOpState Keep Keep Keep Always 0 0 0)
      (StencilOpState Keep Keep Keep Always 0 0 0)
      0 0)
    (PipelineColorBlendStateCreateInfo zeroBits False ClearOp
      [PipelineColorBlendAttachmentState False Zero Zero Add Zero Zero Add Vk.all]
      $ V.replicate 0)
    (Just $ PipelineDynamicStateCreateInfo zeroBits [viewport, Scissor])
    pipelineLayout
    renderPass
    0
    nullHandle
    (-1)

  memPtr <- mapMemory device mem 0 (size (memReq :: MemoryRequirements)) zeroBits

  print ( dsLayout
        , pipelineLayout
        , vertexShaderModule
        , fragmentShaderModule
        , pipeline
        , memReq
        , memProp
        , memTypeIndex
        , mem
        , memPtr
        )

  unmapMemory device mem

  showWindow window

  let loop = do
        semaphore <- createSemaphore device
        print semaphore
        imageIndex <- acquireNextImage device swapchain semaphore
        let image = images !! fromIntegral imageIndex
            framebuffer = framebuffers !! fromIntegral imageIndex
        print imageIndex
        beginCommandBuffer commandBuffer
        cmdPipelineBarrier commandBuffer AllCommands BottomOfPipe zeroBits
          [] [] [ImageMemoryBarrier zeroBits ColorAttachmentWrite
                 UndefinedLayout ColorAttachmentOptimal
                 ignored ignored
                 image
                 (ImageSubresourceRange Color 0 1 0 1)
                ]
        cmdBeginRenderPass commandBuffer (RenderPassBeginInfo renderPass framebuffer
                                          (Rect2D (Offset2D 0 0) extent) [ClearColor $ FloatColor (V.replicate (0.3 * fromIntegral imageIndex))])
          Inline
        cmdEndRenderPass commandBuffer
        cmdPipelineBarrier commandBuffer AllCommands BottomOfPipe zeroBits
          [] [] [ImageMemoryBarrier ColorAttachmentWrite MemoryRead
                 ColorAttachmentOptimal PresentSource
                 ignored ignored
                 image
                 (ImageSubresourceRange Color 0 1 0 1)
                ]
        endCommandBuffer commandBuffer
        queueSubmit queue [SubmitInfo [(semaphore, BottomOfPipe)] [commandBuffer] []] nullHandle
        queuePresent queue (PresentInfo [] [(swapchain, imageIndex)])
        queueWaitIdle queue
        destroySemaphore device semaphore
        e <- fmap eventPayload <$> pollEvents
        unless (QuitEvent `elem` e) loop
  loop

main :: IO ()
main = do
  HintFramebufferAcceleration $= Disable3D
  bracket (initialize [InitVideo]) (const quit)
    (\() -> bracket (createWindow "Vulkan Test" defaultWindow) destroyWindow run)
