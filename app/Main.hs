{-# LANGUAGE OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Main where

import Lib

import Control.Exception
import Control.Monad
import SDL
import SDL.Video.Vulkan

run :: Window -> IO ()
run window = do
  iext <- instanceExtensions
  let
    app = ApplicationInfo "vulkan-test" (Version 0 0 0) "none" (Version 0 0 0) (Version 1 0 3)
    ici = InstanceCreateInfo app [] iext
  inst <- createInstance ici
  surface <- createSurface window inst
  pd <- physicalDevices inst
  queues <- mapM (\d -> do
                     qfp <- queueFamilyProperties d
                     mapM (\qf -> do
                              s <- queueFamilySupportsPresent (queueFamily qf) surface
                              return (qf, s)
                          ) qfp
                 ) pd
  print queues
  showWindow window
  let loop = do
        e <- fmap eventPayload <$> pollEvents
        unless (QuitEvent `elem` e) loop
  loop

main :: IO ()
main =
  bracket (initialize [InitVideo]) (const quit)
    (\() -> bracket (createWindow "Vulkan Test" defaultWindow) destroyWindow run)
