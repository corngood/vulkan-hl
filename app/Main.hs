{-# language OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad
import Graphics.Vulkan.HL
import SDL hiding (Surface)
import SDL.Video.Vulkan

findAndCreateDevice :: Instance -> Surface -> IO (PhysicalDevice, QueueFamily, Device, Queue, CommandPool)
findAndCreateDevice inst surface = do
  m <- findDevice =<< physicalDevices inst
  case m of
    Just (pd, qf) -> do
      d <- createDevice pd $ DeviceCreateInfo [QueueCreateInfo qf 1] [] ["VK_KHR_swapchain"]
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

run :: Window -> IO ()
run window = do
  iext <- requiredInstanceExtensions
  let
    app = ApplicationInfo "vulkan-test" (Version 0 0 0) "none" (Version 0 0 0) (Version 1 0 3)
    ici = InstanceCreateInfo app [] iext
  inst <- createInstance ici
  surface <- createSurface window inst
  (physicalDevice, _, device, queue, commandPool) <- findAndCreateDevice inst surface
  surfaceFormats physicalDevice surface >>= print
  print (device, queue, commandPool)
  showWindow window
  let loop = do
        e <- fmap eventPayload <$> pollEvents
        unless (QuitEvent `elem` e) loop
  loop

main :: IO ()
main = do
  HintFramebufferAcceleration $= Disable3D
  bracket (initialize [InitVideo]) (const quit)
    (\() -> bracket (createWindow "Vulkan Test" defaultWindow) destroyWindow run)
