{-# language OverloadedStrings #-}

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

findAndCreateDevice :: Instance -> Surface -> IO (PhysicalDevice, QueueFamily, Device, Queue, CommandPool)
findAndCreateDevice inst surface = do
  m <- findDevice =<< physicalDevices inst
  case m of
    Just (pd, qf) -> do
      d <- createDevice pd $ DeviceCreateInfo [QueueCreateInfo qf 1] ["VK_LAYER_LUNARG_standard_validation"] ["VK_KHR_swapchain"]
      q <- getQueue d qf 0
      cp <- createCommandPool d $ CommandPoolCreateInfo qf
      return (pd, qf, d, q, cp)
    Nothing -> error "No Device"
  where
    findDevice [] = return Nothing
    findDevice (d:ds) = do
      x <- findQueue =<< queueFamilyProperties d
      case x of
        Just y -> return $ Just y
        Nothing -> findDevice ds
      where
        findQueue [] = return Nothing
        findQueue (q:qs) = do
          let qf = queueFamily q
          s <- queueFamilySupportsPresent qf surface
          if queueGraphics (queueFlags q) && s
            then return $ Just (d, qf)
            else findQueue qs

findSurfaceFormat :: [Vk.SurfaceFormat] -> Vk.SurfaceFormat
findSurfaceFormat [Vk.SurfaceFormat Vk.FormatUndefined cs] = Vk.SurfaceFormat Vk.FormatB8g8r8a8Srgb cs
findSurfaceFormat (sf:_) = sf
findSurfaceFormat [] = error "no surface format"

swapChainExtents :: Window -> Vk.SurfaceCapabilities -> IO Vk.Extent2D
swapChainExtents w sc =
  case Vk.currentExtent sc of
    Vk.Extent2D (-1) _ -> (\(V2 x y) -> Vk.Extent2D (fromIntegral x) (fromIntegral y)) <$> (get $ windowSize w)
    p -> return p

swapChainImageCount :: Vk.SurfaceCapabilities -> Int
swapChainImageCount sc =
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
    ici = InstanceCreateInfo app ["VK_LAYER_LUNARG_standard_validation"] ("VK_KHR_surface" : iext)
  inst <- createInstance ici
  surface <- createSurface window inst
  (physicalDevice, qf, device, queue, commandPool) <- findAndCreateDevice inst surface
  surfaceFormat <- findSurfaceFormat <$> surfaceFormats physicalDevice surface
  surfaceCaps <- surfaceCapabilities physicalDevice surface
  commandBuffer <- allocateCommandBuffer device commandPool Vk.CommandBufferLevelPrimary
  extent <- swapChainExtents window surfaceCaps
  let imageCount = swapChainImageCount surfaceCaps
  swapchain <- createSwapchain device (SwapchainCreateInfo (Vk.SwapchainCreateFlags zeroBits) surface imageCount surfaceFormat
                                       extent 1 Vk.ImageUsageColorAttachmentBit Vk.SharingModeExclusive
                                       [qf] (Vk.currentTransform surfaceCaps) Vk.CompositeAlphaOpaqueBit
                                       Vk.PresentModeFifo True)
  print (device, queue, commandPool, surfaceFormat, commandBuffer, surfaceCaps, extent, imageCount, swapchain)
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
