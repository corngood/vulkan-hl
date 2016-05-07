{-# language StandaloneDeriving #-}
{-# options_ghc -fno-warn-orphans #-}

module Graphics.Vulkan.HL.Internal.Orphan where

import Graphics.Vulkan

deriving instance Show CommandPool
deriving instance Show Extent2D
deriving instance Show Extent3D
deriving instance Show QueueFamilyProperties
deriving instance Show Surface
deriving instance Show SurfaceCapabilities
deriving instance Show SurfaceFormat
deriving instance Show Swapchain
