{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Bits
import Data.Vector.Storable.Sized as V
import Graphics.Vulkan as Vk
import Linear.V2
import SDL hiding (Surface)
import SDL.Video.Vulkan
import System.Environment

findAndCreateDevice :: Instance -> Surface -> IO (PhysicalDevice, Int, Device, Queue, CommandPool)
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
          if hasFlag (flags (q :: QueueFamilyProperties)) GraphicsQueue && s
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

swapchainImageCount :: SurfaceCapabilities -> Int
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
     Vk.Information .|.
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

  showWindow window

  let loop = do
        semaphore <- createSemaphore device
        print semaphore
        imageIndex <- acquireNextImage device swapchain semaphore
        print imageIndex
        beginCommandBuffer commandBuffer
        cmdPipelineBarrier commandBuffer AllCommands BottomOfPipe zeroBits
          [] [] [ImageMemoryBarrier zeroBits ColorAttachmentWrite
                 UndefinedLayout ColorAttachmentOptimal
                 Ignored Ignored
                 (images !! imageIndex)
                 (ImageSubresourceRange Color 0 1 0 1)
                ]
        cmdBeginRenderPass commandBuffer (RenderPassBeginInfo renderPass (framebuffers !! imageIndex)
                                          (Rect2D (Offset2D 0 0) extent) [ClearColor $ FloatColor (V.replicate (0.3 * fromIntegral imageIndex))])
          Inline
        cmdEndRenderPass commandBuffer
        cmdPipelineBarrier commandBuffer AllCommands BottomOfPipe zeroBits
          [] [] [ImageMemoryBarrier ColorAttachmentWrite MemoryRead
                 ColorAttachmentOptimal PresentSource
                 Ignored Ignored
                 (images !! imageIndex)
                 (ImageSubresourceRange Color 0 1 0 1)
                ]
        endCommandBuffer commandBuffer
        queueSubmit queue [SubmitInfo [(semaphore, BottomOfPipe)] [commandBuffer] []] nullFence
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
