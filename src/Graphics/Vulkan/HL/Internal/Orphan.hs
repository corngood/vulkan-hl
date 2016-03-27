{-# language StandaloneDeriving #-}
{-# options_ghc -fno-warn-orphans #-}

module Graphics.Vulkan.HL.Internal.Orphan where

import Graphics.Vulkan

deriving instance Ord VkCommandPool
deriving instance Show VkCommandPool
deriving instance Show VkExtent3D
deriving instance Show VkQueueFamilyProperties
deriving instance Show VkSurfaceKHR
