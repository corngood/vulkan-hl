{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Main where

import Prelude hiding (readFile)

import Control.Exception
import Control.Monad
import Data.Bits
import Data.ByteString (readFile)
import qualified Data.Vector.Storable.Sized as V (replicate)
import Data.Word
import Foreign.C (CFloat)
import Foreign.Marshal (pokeArray)
import Foreign.Ptr (castPtr)
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
findSurfaceFormat [SurfaceFormat Undefined cs] = SurfaceFormat B8G8R8A8Srgb cs
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
  extent@(Extent2D width height) <- swapchainExtents window surfaceCaps
  let imageCount = swapchainImageCount surfaceCaps
  swapchain <- createSwapchain device (SwapchainCreateInfo zeroBits surface imageCount surfaceFormat
                                       extent 1 ImageColorAttachment Exclusive
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
      [VertexInputBindingDescription 0 (5 * 4) PerVertex]
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

  memProp <- memoryProperties physicalDevice

  buffer <- createBuffer device $ BufferCreateInfo zeroBits (3 * 5 * 4) VertexBuffer Exclusive []
  memReq <- memoryRequirements device buffer

  let findMemType :: MemoryRequirements -> MemoryProperty -> Word
      findMemType mr f = find 0 (memoryType memProp) mr f
        where
          find i ((MemoryType p _):ms) (MemoryRequirements _ _ t) f | hasFlags p f && testBit t (fromIntegral i) = i
          find i (m:ms) mr f = find (succ i) ms mr f

  mem <- allocate device $ MemoryAllocateInfo (size (memReq :: MemoryRequirements)) $ findMemType memReq HostVisible

  memPtr <- mapMemory device mem 0 (size (memReq :: MemoryRequirements)) zeroBits

  pokeArray (castPtr memPtr)
    ([-1, -1, 0.25, 0, 0,
      1, -1, 0.25, 1, 0,
      0, 1, 1, 0.5, 1] :: [CFloat])

  unmapMemory device mem

  bindMemory device buffer mem 0

  print ( dsLayout
        , pipelineLayout
        , vertexShaderModule
        , fragmentShaderModule
        , pipeline
        , buffer
        , memReq
        , memProp
        , mem
        )

  descriptorPool <- createDescriptorPool device $ DescriptorPoolCreateInfo zeroBits 1 [DescriptorPoolSize CombinedImageSampler 1]

  descriptorSet <- allocateDescriptorSet device descriptorPool dsLayout

  sampler <- createSampler device $ SamplerCreateInfo
    zeroBits
    LinearFilter
    LinearFilter
    LinearMip
    Repeat
    Repeat
    Repeat
    0
    False
    0
    False
    Always
    0
    0
    FloatOpaqueWhite
    False

  image <- createImage device $ ImageCreateInfo
    zeroBits
    Dim2
    B8G8R8A8UNorm
    (Extent3D 4 4 1)
    1
    1
    Samples1
    Linear
    ImageSampled
    Exclusive
    []
    Preinitialized

  memReq <- memoryRequirements device image

  let memSize = size (memReq :: MemoryRequirements)

  mem <- allocate device $ MemoryAllocateInfo memSize $ findMemType memReq HostVisible

  memPtr <- mapMemory device mem 0 memSize zeroBits

  pokeArray (castPtr memPtr) $ replicate (fromIntegral memSize) (255 :: Word8)

  unmapMemory device mem

  bindMemory device image mem 0

  commandBuffer <- allocateCommandBuffer device commandPool Primary

  beginCommandBuffer commandBuffer

  cmdPipelineBarrier commandBuffer TopOfPipe TopOfPipe zeroBits [] []
    [ImageMemoryBarrier HostWrite ShaderRead Preinitialized ShaderReadOnlyOptimal ignored ignored image $
      ImageSubresourceRange Color 0 1 0 1]

  endCommandBuffer commandBuffer

  queueSubmit queue [SubmitInfo [] [commandBuffer] []] nullHandle
  queueWaitIdle queue

  imageView <- createImageView device $ ImageViewCreateInfo
    zeroBits
    image
    Type2D
    B8G8R8A8UNorm
    (ComponentMapping R G B A)
    (ImageSubresourceRange Color 0 1 0 1)

  -- freeCommandBuffer

  updateDescriptorSets device
    [ WriteDescriptorSet descriptorSet 0 0 1 CombinedImageSampler
      (Just [DescriptorImageInfo sampler imageView ShaderReadOnlyOptimal])
      Nothing
      Nothing
    ] []

  print ( descriptorPool
        , descriptorSet
        , sampler
        , image
        , memReq
        , mem
        , imageView
        )

  showWindow window

  commandBuffer <- allocateCommandBuffer device commandPool Primary

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
                                          (Rect2D (Offset2D 0 0) extent) [ClearColor $ FloatColor (V.replicate 0.3)])
          Inline
        cmdBindPipeline commandBuffer GraphicsBindPoint pipeline
        cmdBindDescriptorSets commandBuffer GraphicsBindPoint pipelineLayout 0 [descriptorSet] []
        cmdSetViewport commandBuffer 0 $ Viewport 0 0 (fromIntegral width) (fromIntegral height) 0 1
        cmdSetScissor commandBuffer 0 $ Rect2D (Offset2D 0 0) (Extent2D width height)
        cmdBindVertexBuffer commandBuffer 0 buffer 0
        cmdDraw commandBuffer 3 1 0 0
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
