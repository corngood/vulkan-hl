{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Main where

import Control.Exception
-- import Control.Monad
import Data.Bits
import qualified Graphics.Vulkan as Vk
import qualified Graphics.Vulkan.KHR.Surface as VkS
import Graphics.Vulkan.HL
import Linear.V2
import SDL hiding (Surface)
import SDL.Video.Vulkan
import System.Environment

findAndCreateDevice :: Vk.Instance -> Vk.Surface -> IO (Vk.PhysicalDevice, Int, Vk.Device, Vk.Queue, Vk.CommandPool)
findAndCreateDevice inst surface = do
  m <- findDevice =<< physicalDevices inst
  case m of
    Just (pd, qf) -> do
      d <- createDevice pd $ DeviceCreateInfo [QueueCreateInfo qf [0]] ["VK_LAYER_LUNARG_standard_validation"] ["VK_KHR_swapchain"]
      q <- getQueue d qf 0
      cp <- createCommandPool d $ CommandPoolCreateInfo qf
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
          if hasFlag (Vk.queueFlags q) Vk.QueueGraphicsBit && s
            then return $ Just (d, i)
            else findQueue (succ i) qs

findSurfaceFormat :: [Vk.SurfaceFormat] -> Vk.SurfaceFormat
findSurfaceFormat [Vk.SurfaceFormat Vk.FormatUndefined cs] = Vk.SurfaceFormat Vk.FormatB8g8r8a8Srgb cs
findSurfaceFormat (sf:_) = sf
findSurfaceFormat [] = error "no surface format"

swapchainExtents :: Window -> Vk.SurfaceCapabilities -> IO Vk.Extent2D
swapchainExtents w sc =
  case Vk.currentExtent sc of
    Vk.Extent2D (-1) _ -> (\(V2 x y) -> Vk.Extent2D (fromIntegral x) (fromIntegral y)) <$> (get $ windowSize w)
    p -> return p

swapchainImageCount :: Vk.SurfaceCapabilities -> Int
swapchainImageCount sc =
  let ic = VkS.minImageCount sc + 1
  in fromIntegral $ case Vk.maxImageCount sc of
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
    (Vk.DebugReportErrorBit .|.
     Vk.DebugReportWarningBit .|.
     Vk.DebugReportPerformanceWarningBit .|.
     Vk.DebugReportInformationBit .|.
     Vk.DebugReportDebugBit)
    (\f ot o l mc lp m -> print (f, ot, o, l, mc, lp, m) >> return True)
  surface <- createSurface window inst
  (physicalDevice, qf, device, queue, commandPool) <- findAndCreateDevice inst surface
  surfaceFormat <- findSurfaceFormat <$> surfaceFormats physicalDevice surface
  surfaceCaps <- surfaceCapabilities physicalDevice surface
  commandBuffer <- allocateCommandBuffer device commandPool Vk.CommandBufferLevelPrimary
  extent <- swapchainExtents window surfaceCaps
  let imageCount = swapchainImageCount surfaceCaps
  swapchain <- createSwapchain device (SwapchainCreateInfo (Vk.SwapchainCreateFlags zeroBits) surface imageCount surfaceFormat
                                       extent 1 Vk.ImageUsageColorAttachmentBit Vk.SharingModeExclusive
                                       [qf] (Vk.currentTransform surfaceCaps) Vk.CompositeAlphaOpaqueBit
                                       Vk.PresentModeFifo True)
  images <- swapchainImages device swapchain
  imageViews <- mapM
    (\image -> createImageView device $ ImageViewCreateInfo (Vk.ImageViewCreateFlags zeroBits) image Vk.ImageViewType2d
               (Vk.format (surfaceFormat :: Vk.SurfaceFormat))
               (Vk.ComponentMapping Vk.ComponentSwizzleR Vk.ComponentSwizzleG Vk.ComponentSwizzleB Vk.ComponentSwizzleA)
               (Vk.ImageSubresourceRange Vk.ImageAspectColorBit 0 1 0 1)) images
  let attachment = Vk.AttachmentDescription (Vk.AttachmentDescriptionFlags zeroBits)
        (Vk.format (surfaceFormat :: Vk.SurfaceFormat)) Vk.SampleCount1Bit Vk.AttachmentLoadOpClear Vk.AttachmentStoreOpStore
        Vk.AttachmentLoadOpDontCare Vk.AttachmentStoreOpDontCare Vk.ImageLayoutColorAttachmentOptimal
        Vk.ImageLayoutColorAttachmentOptimal
      subpass = SubpassDescription (Vk.SubpassDescriptionFlags zeroBits) Vk.PipelineBindPointGraphics
        [] [Vk.AttachmentReference 0 Vk.ImageLayoutColorAttachmentOptimal] []
  renderPass <- createRenderPass device (RenderPassCreateInfo (Vk.RenderPassCreateFlags zeroBits) [attachment] [subpass] [])
  print (device, queue, commandPool, surfaceFormat, commandBuffer, surfaceCaps, extent, imageCount, swapchain, images, cb, imageViews, renderPass)
  showWindow window
  -- let loop = do
  --       e <- fmap eventPayload <$> pollEvents
  --       unless (QuitEvent `elem` e) loop
  -- loop

main :: IO ()
main = do
  HintFramebufferAcceleration $= Disable3D
  bracket (initialize [InitVideo]) (const quit)
    (\() -> bracket (createWindow "Vulkan Test" defaultWindow) destroyWindow run)
