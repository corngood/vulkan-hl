{-# language StandaloneDeriving #-}
{-# options_ghc -fno-warn-orphans #-}

module Graphics.Vulkan.HL.Internal.Orphan where

import Graphics.Vulkan

deriving instance Ord VkColorSpaceKHR
deriving instance Ord CommandPool
deriving instance Ord VkFormat
deriving instance Ord VkSurfaceFormatKHR
deriving instance Show CommandPool
deriving instance Show VkExtent3D
deriving instance Show VkQueueFamilyProperties
deriving instance Show VkSurfaceFormatKHR
deriving instance Show SurfaceKHR
