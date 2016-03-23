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
  instanceExtensions >>= print
  createInstance ici >>= print
  window <- createWindow "Vulkan Test" defaultWindow
  showWindow window
  let loop = do
        e <- fmap eventPayload <$> pollEvents
        unless (QuitEvent `elem` e) loop
  loop
  destroyWindow window
  quit
  where
    app = ApplicationInfo "vulkan-test" (Version 0 0 0) "none" (Version 0 0 0) (Version 1 0 3)
    ici = InstanceCreateInfo app [] []
