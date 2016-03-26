{-# LANGUAGE OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Main where

import Lib

import Control.Monad
import SDL
import SDL.Video.Vulkan

main :: IO ()
main = do
  initialize [InitVideo]
  window <- createWindow "Vulkan Test" defaultWindow
  iext <- instanceExtensions
  let
    app = ApplicationInfo "vulkan-test" (Version 0 0 0) "none" (Version 0 0 0) (Version 1 0 3)
    ici = InstanceCreateInfo app [] iext
  inst <- createInstance ici
  surface <- createSurface window inst
  print surface
  queues <- physicalDevices inst >>= sequence . (fmap queueFamilyProperties)
  print queues
  showWindow window
  let loop = do
        e <- fmap eventPayload <$> pollEvents
        unless (QuitEvent `elem` e) loop
  loop
  destroyWindow window
  quit
