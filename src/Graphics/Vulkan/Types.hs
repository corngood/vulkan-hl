{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language DataKinds #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# options_ghc -Wno-missing-pattern-synonym-signatures #-}

module Graphics.Vulkan.Types where

import Data.Bits
import Data.ByteString (ByteString)
import Data.Int
import Data.Vector.Storable.Sized as V hiding (head, length)
import Data.Void
import Data.Word
import Foreign.C
import Foreign.Ptr
import Graphics.Vulkan.Internal.Marshal
import Graphics.Vulkan.Raw

type LayerName = String
type ExtensionName = String
data Version = Version Word Word Word

data ApplicationInfo = ApplicationInfo { applicationName :: String
                                       , applicationVersion :: Version
                                       , engineName :: String
                                       , engineVersion :: Version
                                       , apiVersion :: Version
                                       }

data InstanceCreateInfo = InstanceCreateInfo { applicationInfo :: ApplicationInfo
                                             , instanceLayers :: [LayerName]
                                             , instanceExtensions :: [ExtensionName]
                                             }

data Extension = Extension { extensionName :: ExtensionName
                           , extensionVersion :: Word
                           }
               deriving (Eq, Ord, Show, Read)

type Instance = Handle VkInstance

type PhysicalDevice = Handle VkPhysicalDevice

type QueueFlags = Flags VkQueueFlags

pattern GraphicsQueue = Flags VK_QUEUE_GRAPHICS_BIT :: QueueFlags

data Extent2D = Extent2D { width :: Word
                         , height :: Word
                         }
              deriving (Eq, Ord, Show)

data Extent3D = Extent3D { width :: Word
                         , height :: Word
                         , depth :: Word
                         }
              deriving (Eq, Ord, Show)

data QueueFamilyProperties = QueueFamilyProperties { flags :: QueueFlags
                                                   , count :: Word
                                                   , timestampValidBits :: Word
                                                   , minImageTransferGranularity :: Extent3D
                                                   }
                           deriving (Eq, Ord, Show)

type Surface = Handle VkSurfaceKHR

data QueueCreateInfo = QueueCreateInfo { queueCreateFamily :: Word
                                       , queuePriorities :: [Float]
                                       }

data DeviceCreateInfo = DeviceCreateInfo { deviceQueues :: [QueueCreateInfo]
                                         , deviceLayers :: [LayerName]
                                         , deviceExtensions :: [ExtensionName]
                                         }

type Device = Handle VkDevice

type Queue = Handle VkQueue

type CommandPoolCreateFlags = Flags VkCommandPoolCreateFlags

pattern CreateResetCommandBuffer = Flags VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT :: CommandPoolCreateFlags

data CommandPoolCreateInfo = CommandPoolCreateInfo { flags :: CommandPoolCreateFlags
                                                   , queueFamilyIndex :: Word
                                                   }

type CommandPool = Handle VkCommandPool

type Format = Enumerator VkFormat

pattern Undefined = Enumerator VK_FORMAT_UNDEFINED :: Format
pattern R4G4UNormPack8 = Enumerator VK_FORMAT_R4G4_UNORM_PACK8 :: Format
pattern R4G4B4A4UNormPack16 = Enumerator VK_FORMAT_R4G4B4A4_UNORM_PACK16 :: Format
pattern B4G4R4A4UNormPack16 = Enumerator VK_FORMAT_B4G4R4A4_UNORM_PACK16 :: Format
pattern R5G6B5UNormPack16 = Enumerator VK_FORMAT_R5G6B5_UNORM_PACK16 :: Format
pattern B5G6R5UNormPack16 = Enumerator VK_FORMAT_B5G6R5_UNORM_PACK16 :: Format
pattern R5G5B5A1UNormPack16 = Enumerator VK_FORMAT_R5G5B5A1_UNORM_PACK16 :: Format
pattern B5G5R5A1UNormPack16 = Enumerator VK_FORMAT_B5G5R5A1_UNORM_PACK16 :: Format
pattern A1R5G5B5UNormPack16 = Enumerator VK_FORMAT_A1R5G5B5_UNORM_PACK16 :: Format
pattern R8UNorm = Enumerator VK_FORMAT_R8_UNORM :: Format
pattern R8SNorm = Enumerator VK_FORMAT_R8_SNORM :: Format
pattern R8Uscaled = Enumerator VK_FORMAT_R8_USCALED :: Format
pattern R8Sscaled = Enumerator VK_FORMAT_R8_SSCALED :: Format
pattern R8UInt = Enumerator VK_FORMAT_R8_UINT :: Format
pattern R8SInt = Enumerator VK_FORMAT_R8_SINT :: Format
pattern R8Srgb = Enumerator VK_FORMAT_R8_SRGB :: Format
pattern R8G8UNorm = Enumerator VK_FORMAT_R8G8_UNORM :: Format
pattern R8G8SNorm = Enumerator VK_FORMAT_R8G8_SNORM :: Format
pattern R8G8Uscaled = Enumerator VK_FORMAT_R8G8_USCALED :: Format
pattern R8G8Sscaled = Enumerator VK_FORMAT_R8G8_SSCALED :: Format
pattern R8G8UInt = Enumerator VK_FORMAT_R8G8_UINT :: Format
pattern R8G8SInt = Enumerator VK_FORMAT_R8G8_SINT :: Format
pattern R8G8Srgb = Enumerator VK_FORMAT_R8G8_SRGB :: Format
pattern R8G8B8UNorm = Enumerator VK_FORMAT_R8G8B8_UNORM :: Format
pattern R8G8B8SNorm = Enumerator VK_FORMAT_R8G8B8_SNORM :: Format
pattern R8G8B8Uscaled = Enumerator VK_FORMAT_R8G8B8_USCALED :: Format
pattern R8G8B8Sscaled = Enumerator VK_FORMAT_R8G8B8_SSCALED :: Format
pattern R8G8B8UInt = Enumerator VK_FORMAT_R8G8B8_UINT :: Format
pattern R8G8B8SInt = Enumerator VK_FORMAT_R8G8B8_SINT :: Format
pattern R8G8B8Srgb = Enumerator VK_FORMAT_R8G8B8_SRGB :: Format
pattern B8G8R8UNorm = Enumerator VK_FORMAT_B8G8R8_UNORM :: Format
pattern B8G8R8SNorm = Enumerator VK_FORMAT_B8G8R8_SNORM :: Format
pattern B8G8R8Uscaled = Enumerator VK_FORMAT_B8G8R8_USCALED :: Format
pattern B8G8R8Sscaled = Enumerator VK_FORMAT_B8G8R8_SSCALED :: Format
pattern B8G8R8UInt = Enumerator VK_FORMAT_B8G8R8_UINT :: Format
pattern B8G8R8SInt = Enumerator VK_FORMAT_B8G8R8_SINT :: Format
pattern B8G8R8Srgb = Enumerator VK_FORMAT_B8G8R8_SRGB :: Format
pattern R8G8B8A8UNorm = Enumerator VK_FORMAT_R8G8B8A8_UNORM :: Format
pattern R8G8B8A8SNorm = Enumerator VK_FORMAT_R8G8B8A8_SNORM :: Format
pattern R8G8B8A8Uscaled = Enumerator VK_FORMAT_R8G8B8A8_USCALED :: Format
pattern R8G8B8A8Sscaled = Enumerator VK_FORMAT_R8G8B8A8_SSCALED :: Format
pattern R8G8B8A8UInt = Enumerator VK_FORMAT_R8G8B8A8_UINT :: Format
pattern R8G8B8A8SInt = Enumerator VK_FORMAT_R8G8B8A8_SINT :: Format
pattern R8G8B8A8Srgb = Enumerator VK_FORMAT_R8G8B8A8_SRGB :: Format
pattern B8G8R8A8UNorm = Enumerator VK_FORMAT_B8G8R8A8_UNORM :: Format
pattern B8G8R8A8SNorm = Enumerator VK_FORMAT_B8G8R8A8_SNORM :: Format
pattern B8G8R8A8Uscaled = Enumerator VK_FORMAT_B8G8R8A8_USCALED :: Format
pattern B8G8R8A8Sscaled = Enumerator VK_FORMAT_B8G8R8A8_SSCALED :: Format
pattern B8G8R8A8UInt = Enumerator VK_FORMAT_B8G8R8A8_UINT :: Format
pattern B8G8R8A8SInt = Enumerator VK_FORMAT_B8G8R8A8_SINT :: Format
pattern B8G8R8A8Srgb = Enumerator VK_FORMAT_B8G8R8A8_SRGB :: Format
pattern A8B8G8R8UNormPack32 = Enumerator VK_FORMAT_A8B8G8R8_UNORM_PACK32 :: Format
pattern A8B8G8R8SNormPack32 = Enumerator VK_FORMAT_A8B8G8R8_SNORM_PACK32 :: Format
pattern A8B8G8R8UscaledPack32 = Enumerator VK_FORMAT_A8B8G8R8_USCALED_PACK32 :: Format
pattern A8B8G8R8SscaledPack32 = Enumerator VK_FORMAT_A8B8G8R8_SSCALED_PACK32 :: Format
pattern A8B8G8R8UIntPack32 = Enumerator VK_FORMAT_A8B8G8R8_UINT_PACK32 :: Format
pattern A8B8G8R8SIntPack32 = Enumerator VK_FORMAT_A8B8G8R8_SINT_PACK32 :: Format
pattern A8B8G8R8SrgbPack32 = Enumerator VK_FORMAT_A8B8G8R8_SRGB_PACK32 :: Format
pattern A2R10G10B10UNormPack32 = Enumerator VK_FORMAT_A2R10G10B10_UNORM_PACK32 :: Format
pattern A2R10G10B10SNormPack32 = Enumerator VK_FORMAT_A2R10G10B10_SNORM_PACK32 :: Format
pattern A2R10G10B10UscaledPack32 = Enumerator VK_FORMAT_A2R10G10B10_USCALED_PACK32 :: Format
pattern A2R10G10B10SscaledPack32 = Enumerator VK_FORMAT_A2R10G10B10_SSCALED_PACK32 :: Format
pattern A2R10G10B10UIntPack32 = Enumerator VK_FORMAT_A2R10G10B10_UINT_PACK32 :: Format
pattern A2R10G10B10SIntPack32 = Enumerator VK_FORMAT_A2R10G10B10_SINT_PACK32 :: Format
pattern A2B10G10R10UNormPack32 = Enumerator VK_FORMAT_A2B10G10R10_UNORM_PACK32 :: Format
pattern A2B10G10R10SNormPack32 = Enumerator VK_FORMAT_A2B10G10R10_SNORM_PACK32 :: Format
pattern A2B10G10R10UscaledPack32 = Enumerator VK_FORMAT_A2B10G10R10_USCALED_PACK32 :: Format
pattern A2B10G10R10SscaledPack32 = Enumerator VK_FORMAT_A2B10G10R10_SSCALED_PACK32 :: Format
pattern A2B10G10R10UIntPack32 = Enumerator VK_FORMAT_A2B10G10R10_UINT_PACK32 :: Format
pattern A2B10G10R10SIntPack32 = Enumerator VK_FORMAT_A2B10G10R10_SINT_PACK32 :: Format
pattern R16UNorm = Enumerator VK_FORMAT_R16_UNORM :: Format
pattern R16SNorm = Enumerator VK_FORMAT_R16_SNORM :: Format
pattern R16Uscaled = Enumerator VK_FORMAT_R16_USCALED :: Format
pattern R16Sscaled = Enumerator VK_FORMAT_R16_SSCALED :: Format
pattern R16UInt = Enumerator VK_FORMAT_R16_UINT :: Format
pattern R16SInt = Enumerator VK_FORMAT_R16_SINT :: Format
pattern R16SFloat = Enumerator VK_FORMAT_R16_SFLOAT :: Format
pattern R16G16UNorm = Enumerator VK_FORMAT_R16G16_UNORM :: Format
pattern R16G16SNorm = Enumerator VK_FORMAT_R16G16_SNORM :: Format
pattern R16G16Uscaled = Enumerator VK_FORMAT_R16G16_USCALED :: Format
pattern R16G16Sscaled = Enumerator VK_FORMAT_R16G16_SSCALED :: Format
pattern R16G16UInt = Enumerator VK_FORMAT_R16G16_UINT :: Format
pattern R16G16SInt = Enumerator VK_FORMAT_R16G16_SINT :: Format
pattern R16G16SFloat = Enumerator VK_FORMAT_R16G16_SFLOAT :: Format
pattern R16G16B16UNorm = Enumerator VK_FORMAT_R16G16B16_UNORM :: Format
pattern R16G16B16SNorm = Enumerator VK_FORMAT_R16G16B16_SNORM :: Format
pattern R16G16B16Uscaled = Enumerator VK_FORMAT_R16G16B16_USCALED :: Format
pattern R16G16B16Sscaled = Enumerator VK_FORMAT_R16G16B16_SSCALED :: Format
pattern R16G16B16UInt = Enumerator VK_FORMAT_R16G16B16_UINT :: Format
pattern R16G16B16SInt = Enumerator VK_FORMAT_R16G16B16_SINT :: Format
pattern R16G16B16SFloat = Enumerator VK_FORMAT_R16G16B16_SFLOAT :: Format
pattern R16G16B16A16UNorm = Enumerator VK_FORMAT_R16G16B16A16_UNORM :: Format
pattern R16G16B16A16SNorm = Enumerator VK_FORMAT_R16G16B16A16_SNORM :: Format
pattern R16G16B16A16Uscaled = Enumerator VK_FORMAT_R16G16B16A16_USCALED :: Format
pattern R16G16B16A16Sscaled = Enumerator VK_FORMAT_R16G16B16A16_SSCALED :: Format
pattern R16G16B16A16UInt = Enumerator VK_FORMAT_R16G16B16A16_UINT :: Format
pattern R16G16B16A16SInt = Enumerator VK_FORMAT_R16G16B16A16_SINT :: Format
pattern R16G16B16A16SFloat = Enumerator VK_FORMAT_R16G16B16A16_SFLOAT :: Format
pattern R32UInt = Enumerator VK_FORMAT_R32_UINT :: Format
pattern R32SInt = Enumerator VK_FORMAT_R32_SINT :: Format
pattern R32SFloat = Enumerator VK_FORMAT_R32_SFLOAT :: Format
pattern R32G32UInt = Enumerator VK_FORMAT_R32G32_UINT :: Format
pattern R32G32SInt = Enumerator VK_FORMAT_R32G32_SINT :: Format
pattern R32G32SFloat = Enumerator VK_FORMAT_R32G32_SFLOAT :: Format
pattern R32G32B32UInt = Enumerator VK_FORMAT_R32G32B32_UINT :: Format
pattern R32G32B32SInt = Enumerator VK_FORMAT_R32G32B32_SINT :: Format
pattern R32G32B32SFloat = Enumerator VK_FORMAT_R32G32B32_SFLOAT :: Format
pattern R32G32B32A32UInt = Enumerator VK_FORMAT_R32G32B32A32_UINT :: Format
pattern R32G32B32A32SInt = Enumerator VK_FORMAT_R32G32B32A32_SINT :: Format
pattern R32G32B32A32SFloat = Enumerator VK_FORMAT_R32G32B32A32_SFLOAT :: Format
pattern R64UInt = Enumerator VK_FORMAT_R64_UINT :: Format
pattern R64SInt = Enumerator VK_FORMAT_R64_SINT :: Format
pattern R64SFloat = Enumerator VK_FORMAT_R64_SFLOAT :: Format
pattern R64G64UInt = Enumerator VK_FORMAT_R64G64_UINT :: Format
pattern R64G64SInt = Enumerator VK_FORMAT_R64G64_SINT :: Format
pattern R64G64SFloat = Enumerator VK_FORMAT_R64G64_SFLOAT :: Format
pattern R64G64B64UInt = Enumerator VK_FORMAT_R64G64B64_UINT :: Format
pattern R64G64B64SInt = Enumerator VK_FORMAT_R64G64B64_SINT :: Format
pattern R64G64B64SFloat = Enumerator VK_FORMAT_R64G64B64_SFLOAT :: Format
pattern R64G64B64A64UInt = Enumerator VK_FORMAT_R64G64B64A64_UINT :: Format
pattern R64G64B64A64SInt = Enumerator VK_FORMAT_R64G64B64A64_SINT :: Format
pattern R64G64B64A64SFloat = Enumerator VK_FORMAT_R64G64B64A64_SFLOAT :: Format
pattern B10G11R11UFloatPack32 = Enumerator VK_FORMAT_B10G11R11_UFLOAT_PACK32 :: Format
pattern E5B9G9R9UFloatPack32 = Enumerator VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 :: Format
pattern D16UNorm = Enumerator VK_FORMAT_D16_UNORM :: Format
pattern X8D24UNormPack32 = Enumerator VK_FORMAT_X8_D24_UNORM_PACK32 :: Format
pattern D32SFloat = Enumerator VK_FORMAT_D32_SFLOAT :: Format
pattern S8UInt = Enumerator VK_FORMAT_S8_UINT :: Format
pattern D16UNormS8UInt = Enumerator VK_FORMAT_D16_UNORM_S8_UINT :: Format
pattern D24UNormS8UInt = Enumerator VK_FORMAT_D24_UNORM_S8_UINT :: Format
pattern D32SFloatS8UInt = Enumerator VK_FORMAT_D32_SFLOAT_S8_UINT :: Format
pattern BC1RgbUNormBlock = Enumerator VK_FORMAT_BC1_RGB_UNORM_BLOCK :: Format
pattern BC1RgbSrgbBlock = Enumerator VK_FORMAT_BC1_RGB_SRGB_BLOCK :: Format
pattern BC1RgbaUNormBlock = Enumerator VK_FORMAT_BC1_RGBA_UNORM_BLOCK :: Format
pattern BC1RgbaSrgbBlock = Enumerator VK_FORMAT_BC1_RGBA_SRGB_BLOCK :: Format
pattern BC2UNormBlock = Enumerator VK_FORMAT_BC2_UNORM_BLOCK :: Format
pattern BC2SrgbBlock = Enumerator VK_FORMAT_BC2_SRGB_BLOCK :: Format
pattern BC3UNormBlock = Enumerator VK_FORMAT_BC3_UNORM_BLOCK :: Format
pattern BC3SrgbBlock = Enumerator VK_FORMAT_BC3_SRGB_BLOCK :: Format
pattern BC4UNormBlock = Enumerator VK_FORMAT_BC4_UNORM_BLOCK :: Format
pattern BC4SNormBlock = Enumerator VK_FORMAT_BC4_SNORM_BLOCK :: Format
pattern BC5UNormBlock = Enumerator VK_FORMAT_BC5_UNORM_BLOCK :: Format
pattern BC5SNormBlock = Enumerator VK_FORMAT_BC5_SNORM_BLOCK :: Format
pattern BC6hUFloatBlock = Enumerator VK_FORMAT_BC6H_UFLOAT_BLOCK :: Format
pattern BC6hSFloatBlock = Enumerator VK_FORMAT_BC6H_SFLOAT_BLOCK :: Format
pattern BC7UNormBlock = Enumerator VK_FORMAT_BC7_UNORM_BLOCK :: Format
pattern BC7SrgbBlock = Enumerator VK_FORMAT_BC7_SRGB_BLOCK :: Format
pattern EtC2R8G8B8UNormBlock = Enumerator VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK :: Format
pattern EtC2R8G8B8SrgbBlock = Enumerator VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK :: Format
pattern EtC2R8G8B8A1UNormBlock = Enumerator VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK :: Format
pattern EtC2R8G8B8A1SrgbBlock = Enumerator VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK :: Format
pattern EtC2R8G8B8A8UNormBlock = Enumerator VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK :: Format
pattern EtC2R8G8B8A8SrgbBlock = Enumerator VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK :: Format
pattern EacR11UNormBlock = Enumerator VK_FORMAT_EAC_R11_UNORM_BLOCK :: Format
pattern EacR11SNormBlock = Enumerator VK_FORMAT_EAC_R11_SNORM_BLOCK :: Format
pattern EacR11G11UNormBlock = Enumerator VK_FORMAT_EAC_R11G11_UNORM_BLOCK :: Format
pattern EacR11G11SNormBlock = Enumerator VK_FORMAT_EAC_R11G11_SNORM_BLOCK :: Format
pattern Astc4x4UNormBlock = Enumerator VK_FORMAT_ASTC_4x4_UNORM_BLOCK :: Format
pattern Astc4x4SrgbBlock = Enumerator VK_FORMAT_ASTC_4x4_SRGB_BLOCK :: Format
pattern Astc5x4UNormBlock = Enumerator VK_FORMAT_ASTC_5x4_UNORM_BLOCK :: Format
pattern Astc5x4SrgbBlock = Enumerator VK_FORMAT_ASTC_5x4_SRGB_BLOCK :: Format
pattern Astc5x5UNormBlock = Enumerator VK_FORMAT_ASTC_5x5_UNORM_BLOCK :: Format
pattern Astc5x5SrgbBlock = Enumerator VK_FORMAT_ASTC_5x5_SRGB_BLOCK :: Format
pattern Astc6x5UNormBlock = Enumerator VK_FORMAT_ASTC_6x5_UNORM_BLOCK :: Format
pattern Astc6x5SrgbBlock = Enumerator VK_FORMAT_ASTC_6x5_SRGB_BLOCK :: Format
pattern Astc6x6UNormBlock = Enumerator VK_FORMAT_ASTC_6x6_UNORM_BLOCK :: Format
pattern Astc6x6SrgbBlock = Enumerator VK_FORMAT_ASTC_6x6_SRGB_BLOCK :: Format
pattern Astc8x5UNormBlock = Enumerator VK_FORMAT_ASTC_8x5_UNORM_BLOCK :: Format
pattern Astc8x5SrgbBlock = Enumerator VK_FORMAT_ASTC_8x5_SRGB_BLOCK :: Format
pattern Astc8x6UNormBlock = Enumerator VK_FORMAT_ASTC_8x6_UNORM_BLOCK :: Format
pattern Astc8x6SrgbBlock = Enumerator VK_FORMAT_ASTC_8x6_SRGB_BLOCK :: Format
pattern Astc8x8UNormBlock = Enumerator VK_FORMAT_ASTC_8x8_UNORM_BLOCK :: Format
pattern Astc8x8SrgbBlock = Enumerator VK_FORMAT_ASTC_8x8_SRGB_BLOCK :: Format
pattern Astc10x5UNormBlock = Enumerator VK_FORMAT_ASTC_10x5_UNORM_BLOCK :: Format
pattern Astc10x5SrgbBlock = Enumerator VK_FORMAT_ASTC_10x5_SRGB_BLOCK :: Format
pattern Astc10x6UNormBlock = Enumerator VK_FORMAT_ASTC_10x6_UNORM_BLOCK :: Format
pattern Astc10x6SrgbBlock = Enumerator VK_FORMAT_ASTC_10x6_SRGB_BLOCK :: Format
pattern Astc10x8UNormBlock = Enumerator VK_FORMAT_ASTC_10x8_UNORM_BLOCK :: Format
pattern Astc10x8SrgbBlock = Enumerator VK_FORMAT_ASTC_10x8_SRGB_BLOCK :: Format
pattern Astc10x10UNormBlock = Enumerator VK_FORMAT_ASTC_10x10_UNORM_BLOCK :: Format
pattern Astc10x10SrgbBlock = Enumerator VK_FORMAT_ASTC_10x10_SRGB_BLOCK :: Format
pattern Astc12x10UNormBlock = Enumerator VK_FORMAT_ASTC_12x10_UNORM_BLOCK :: Format
pattern Astc12x10SrgbBlock = Enumerator VK_FORMAT_ASTC_12x10_SRGB_BLOCK :: Format
pattern Astc12x12UNormBlock = Enumerator VK_FORMAT_ASTC_12x12_UNORM_BLOCK :: Format
pattern Astc12x12SrgbBlock = Enumerator VK_FORMAT_ASTC_12x12_SRGB_BLOCK :: Format

type ColorSpace = Enumerator VkColorSpaceKHR

data SurfaceFormat = SurfaceFormat { format :: Format
                                   , colorSpace :: ColorSpace
                                   }
                   deriving (Eq, Ord, Show)

data SurfaceCapabilities = SurfaceCapabilities { minImageCount :: Word
                                               , maxImageCount :: Word
                                               , currentExtent :: Extent2D
                                               , minImageExtent :: Extent2D
                                               , maxImageExtent :: Extent2D
                                               , maxImageArrayLayers :: Word
                                               , supportedTransforms :: SurfaceTransformFlags
                                               , currentTransform :: SurfaceTransformFlags
                                               , supportedCompositeAlpha :: CompositeAlphaFlags
                                               , supportedUsageFlags :: ImageUsage
                                               }
                         deriving (Eq, Ord, Show)

type CommandBufferLevel = Enumerator VkCommandBufferLevel

pattern Primary = Enumerator VK_COMMAND_BUFFER_LEVEL_PRIMARY :: CommandBufferLevel

type CommandBuffer = Handle VkCommandBuffer

type SwapchainCreateFlags = Flags VkSwapchainCreateFlagsKHR

type ImageUsage = Flags VkImageUsageFlagBits

pattern ImageTransferSrc = Flags VK_IMAGE_USAGE_TRANSFER_SRC_BIT :: ImageUsage
pattern ImageTransferDst = Flags VK_IMAGE_USAGE_TRANSFER_DST_BIT :: ImageUsage
pattern ImageSampled = Flags VK_IMAGE_USAGE_SAMPLED_BIT :: ImageUsage
pattern ImageStorage = Flags VK_IMAGE_USAGE_STORAGE_BIT :: ImageUsage
pattern ImageColorAttachment = Flags VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT :: ImageUsage
pattern ImageDepthStencilAttachment = Flags VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT :: ImageUsage
pattern ImageTransientAttachment = Flags VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT :: ImageUsage
pattern ImageInputAttachment = Flags VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT :: ImageUsage

type SharingMode = Enumerator VkSharingMode

pattern Exclusive = Enumerator VK_SHARING_MODE_EXCLUSIVE :: SharingMode

type SurfaceTransformFlags = Flags VkSurfaceTransformFlagsKHR

type CompositeAlphaFlags = Flags VkCompositeAlphaFlagBitsKHR

pattern Opaque = Flags VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR :: CompositeAlphaFlags

type PresentMode = Enumerator VkPresentModeKHR

pattern Fifo = Enumerator VK_PRESENT_MODE_FIFO_KHR :: PresentMode

data SwapchainCreateInfo = SwapchainCreateInfo { flags :: SwapchainCreateFlags
                                               , surface :: Surface
                                               , minImageCount :: Word
                                               , imageFormat :: SurfaceFormat
                                               , imageExtent :: Extent2D
                                               , imageArrayLayers :: Word
                                               , imageUsage :: ImageUsage
                                               , imageSharingMode :: SharingMode
                                               , queueFamilyIndices :: [Word]
                                               , preTransform :: SurfaceTransformFlags
                                               , compositeAlpha :: CompositeAlphaFlags
                                               , presentMode :: PresentMode
                                               , clipped :: Bool
                                               }

type Swapchain = Handle VkSwapchainKHR

type FNDebugReportCallback =
  (VkDebugReportFlagsEXT ->
     VkDebugReportObjectTypeEXT ->
       Word64 ->
         CSize -> Int32 -> Ptr CChar -> Ptr CChar -> Ptr Void -> IO VkBool32)

type DebugReportCallbackFun =
  (VkDebugReportFlagsEXT ->
     VkDebugReportObjectTypeEXT ->
       Word64 ->
         CSize -> Int32 -> String -> String -> IO Bool)

data DebugReportCallback = DebugReportCallback VkDebugReportCallbackEXT PFN_vkDebugReportCallbackEXT
                         deriving (Eq, Ord, Show)

type FNcreateDebugReportCallback =
  VkInstance ->
  Ptr VkDebugReportCallbackCreateInfoEXT ->
    Ptr VkAllocationCallbacks -> Ptr VkDebugReportCallbackEXT -> IO VkResult

type DebugReportFlags = Flags VkDebugReportFlagsEXT

pattern Error = Flags VK_DEBUG_REPORT_ERROR_BIT_EXT :: DebugReportFlags
pattern Warning = Flags VK_DEBUG_REPORT_WARNING_BIT_EXT :: DebugReportFlags
pattern PerformanceWarning = Flags VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT :: DebugReportFlags
pattern Information = Flags VK_DEBUG_REPORT_INFORMATION_BIT_EXT :: DebugReportFlags
pattern Debug = Flags VK_DEBUG_REPORT_DEBUG_BIT_EXT :: DebugReportFlags

type ImageViewCreateFlags = Flags VkImageViewCreateFlags

type ImageViewType = Enumerator VkImageViewType

pattern Type2D = Enumerator VK_IMAGE_VIEW_TYPE_2D :: ImageViewType

type ComponentSwizzle = Enumerator VkComponentSwizzle

pattern R = Enumerator VK_COMPONENT_SWIZZLE_R :: ComponentSwizzle
pattern G = Enumerator VK_COMPONENT_SWIZZLE_G :: ComponentSwizzle
pattern B = Enumerator VK_COMPONENT_SWIZZLE_B :: ComponentSwizzle
pattern A = Enumerator VK_COMPONENT_SWIZZLE_A :: ComponentSwizzle

data ComponentMapping = ComponentMapping { red :: ComponentSwizzle
                                         , green :: ComponentSwizzle
                                         , blue :: ComponentSwizzle
                                         , alpha :: ComponentSwizzle
                                         }
                      deriving (Eq, Ord, Show)

type ImageAspectFlags = Flags VkImageAspectFlagBits

pattern Color = Flags VK_IMAGE_ASPECT_COLOR_BIT :: ImageAspectFlags

data ImageSubresourceRange = ImageSubresourceRange { aspectMask :: ImageAspectFlags
                                                   , baseMipLevel :: Word
                                                   , levelCount :: Word
                                                   , baseArrayLayer :: Word
                                                   , layerCount :: Word
                                                   }
                           deriving (Eq, Ord, Show)

type Image = Handle VkImage

type ImageCreateFlags = Flags VkImageCreateFlags

type ImageType = Enumerator VkImageType

pattern Dim1 = Enumerator VK_IMAGE_TYPE_1D :: ImageType
pattern Dim2 = Enumerator VK_IMAGE_TYPE_2D :: ImageType
pattern Dim3 = Enumerator VK_IMAGE_TYPE_3D :: ImageType

type SampleCount = Flags VkSampleCountFlags

pattern Samples1 = Flags VK_SAMPLE_COUNT_1_BIT :: SampleCount
pattern Samples2 = Flags VK_SAMPLE_COUNT_2_BIT :: SampleCount
pattern Samples4 = Flags VK_SAMPLE_COUNT_4_BIT :: SampleCount
pattern Samples8 = Flags VK_SAMPLE_COUNT_8_BIT :: SampleCount
pattern Samples16 = Flags VK_SAMPLE_COUNT_16_BIT :: SampleCount
pattern Samples32 = Flags VK_SAMPLE_COUNT_32_BIT :: SampleCount
pattern Samples64 = Flags VK_SAMPLE_COUNT_64_BIT :: SampleCount

type ImageTiling = Enumerator VkImageTiling

pattern Optimal = Enumerator VK_IMAGE_TILING_OPTIMAL :: ImageTiling
pattern Linear = Enumerator VK_IMAGE_TILING_LINEAR :: ImageTiling

data ImageCreateInfo = ImageCreateInfo
  { flags :: ImageCreateFlags
  , imageType :: ImageType
  , format :: Format
  , extent :: Extent3D
  , mipLevels :: Word
  , arrayLayers :: Word
  , samples :: SampleCount
  , tiling :: ImageTiling
  , usage :: ImageUsage
  , sharingMode :: SharingMode
  , queueFamilyIndices :: [Word]
  , initialLayout :: ImageLayout
  }
  deriving (Eq, Ord, Show)

data ImageViewCreateInfo = ImageViewCreateInfo { flags :: ImageViewCreateFlags
                                               , image :: Image
                                               , viewType :: ImageViewType
                                               , format :: Format
                                               , components :: ComponentMapping
                                               , subresourceRange :: ImageSubresourceRange
                                               }

type ImageView = Handle VkImageView

type AttachmentDescriptionFlags = Flags VkAttachmentDescriptionFlagBits

type SampleCountFlags = Flags VkSampleCountFlagBits

pattern Sample1 = Flags VK_SAMPLE_COUNT_1_BIT :: SampleCountFlags

type AttachmentLoadOp = Enumerator VkAttachmentLoadOp

pattern Clear = Enumerator VK_ATTACHMENT_LOAD_OP_CLEAR :: AttachmentLoadOp
pattern DontCareLoad = Enumerator VK_ATTACHMENT_LOAD_OP_DONT_CARE :: AttachmentLoadOp

type AttachmentStoreOp = Enumerator VkAttachmentStoreOp

pattern Store = Enumerator VK_ATTACHMENT_STORE_OP_STORE :: AttachmentStoreOp

pattern DontCareStore = Enumerator VK_ATTACHMENT_STORE_OP_DONT_CARE :: AttachmentStoreOp

type ImageLayout = Enumerator VkImageLayout

pattern UndefinedLayout = Enumerator VK_IMAGE_LAYOUT_UNDEFINED :: ImageLayout
pattern General = Enumerator VK_IMAGE_LAYOUT_GENERAL :: ImageLayout
pattern ColorAttachmentOptimal = Enumerator VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL :: ImageLayout
pattern DepthStencilAttachmentOptimal = Enumerator VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL :: ImageLayout
pattern DepthStencilReadOnlyOptimal = Enumerator VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL :: ImageLayout
pattern ShaderReadOnlyOptimal = Enumerator VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL :: ImageLayout
pattern TransferSrcOptimal = Enumerator VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL :: ImageLayout
pattern TransferDstOptimal = Enumerator VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL :: ImageLayout
pattern Preinitialized = Enumerator VK_IMAGE_LAYOUT_PREINITIALIZED :: ImageLayout
pattern PresentSource = Enumerator VK_IMAGE_LAYOUT_PRESENT_SRC_KHR :: ImageLayout

data AttachmentDescription = AttachmentDescription { flags :: AttachmentDescriptionFlags
                                                   , format :: Format
                                                   , samples :: SampleCountFlags
                                                   , loadOp :: AttachmentLoadOp
                                                   , storeOp :: AttachmentStoreOp
                                                   , stencilLoadOp :: AttachmentLoadOp
                                                   , stencilStoreOp :: AttachmentStoreOp
                                                   , initialLayout :: ImageLayout
                                                   , finalLayout :: ImageLayout
                                                   }
                             deriving (Eq, Ord, Show)

type SubpassDescriptionFlags = Flags VkSubpassDescriptionFlags

type PipelineBindPoint = Enumerator VkPipelineBindPoint

pattern GraphicsBindPoint = Enumerator VK_PIPELINE_BIND_POINT_GRAPHICS :: PipelineBindPoint

data AttachmentReference = AttachmentReference { attachment :: Word
                                               , layout :: ImageLayout
                                               }
                         deriving (Eq, Ord, Show)

  -- TODO hook these up
data SubpassDescription = SubpassDescription { flags :: SubpassDescriptionFlags
                                             , pipelineBindPoint :: PipelineBindPoint
                                             , inputAttachments :: [AttachmentReference]
                                             , colorAttachments :: [AttachmentReference]
                                             -- , pResolveAttachments :: Maybe [AttachmentReference]
                                             -- , depthStencilAttachment :: VkAttachmentReference
                                             , preserveAttachments :: [Word]
                                             }
                        deriving (Eq, Ord, Show)

type RenderPassCreateFlags = Flags VkRenderPassCreateFlags

data RenderPassCreateInfo = RenderPassCreateInfo { flags :: RenderPassCreateFlags
                                                 , attachments :: [AttachmentDescription]
                                                 , subpasses :: [SubpassDescription]
                                                 , dependencies :: [VkSubpassDependency]
                                                 }

type RenderPass = Handle VkRenderPass

type FramebufferCreateFlags = Flags VkFramebufferCreateFlags

data FramebufferCreateInfo = FramebufferCreateInfo { flags :: FramebufferCreateFlags
                                                   , renderPass :: RenderPass
                                                   , attachments :: [ImageView]
                                                   , width :: Word
                                                   , height :: Word
                                                   , layers :: Word
                                                   }

type Framebuffer = Handle VkFramebuffer

type Semaphore = Handle VkSemaphore

type Fence = Handle VkFence

type Access = Flags VkAccessFlags

pattern IndirectCommandRead = Flags VK_ACCESS_INDIRECT_COMMAND_READ_BIT :: Access
pattern IndexRead = Flags VK_ACCESS_INDEX_READ_BIT :: Access
pattern VertexAttributeRead = Flags VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT :: Access
pattern UniformRead = Flags VK_ACCESS_UNIFORM_READ_BIT :: Access
pattern InputAttachmentRead = Flags VK_ACCESS_INPUT_ATTACHMENT_READ_BIT :: Access
pattern ShaderRead = Flags VK_ACCESS_SHADER_READ_BIT :: Access
pattern ShaderWrite = Flags VK_ACCESS_SHADER_WRITE_BIT :: Access
pattern ColorAttachmentRead = Flags VK_ACCESS_COLOR_ATTACHMENT_READ_BIT :: Access
pattern ColorAttachmentWrite = Flags VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT :: Access
pattern DepthStencilAttachmentRead = Flags VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT :: Access
pattern DepthStencilAttachmentWrite = Flags VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT :: Access
pattern TransferRead = Flags VK_ACCESS_TRANSFER_READ_BIT :: Access
pattern TransferWrite = Flags VK_ACCESS_TRANSFER_WRITE_BIT :: Access
pattern HostRead = Flags VK_ACCESS_HOST_READ_BIT :: Access
pattern HostWrite = Flags VK_ACCESS_HOST_WRITE_BIT :: Access
pattern MemoryRead = Flags VK_ACCESS_MEMORY_READ_BIT :: Access
pattern MemoryWrite = Flags VK_ACCESS_MEMORY_WRITE_BIT :: Access

data MemoryBarrier = MemoryBarrier { srcAccessMask :: Access
                                   , dstAccessMask :: Access
                                   }
                   deriving (Eq, Ord, Show)

type Buffer = Handle VkBuffer

newtype DeviceSize = DeviceSize Word64
  deriving (Eq, Ord, Show, Read, Enum, Real, Bounded, Integral, Num)

data BufferMemoryBarrier = BufferMemoryBarrier { srcAccessMask :: Access
                                               , dstAccessMask :: Access
                                               , srcQueueFamilyIndex :: Word
                                               , dstQueueFamilyIndex :: Word
                                               , buffer :: Buffer
                                               , offset :: DeviceSize
                                               , size :: DeviceSize
                                               }
                         deriving (Eq, Ord, Show)

data ImageMemoryBarrier = ImageMemoryBarrier { srcAccessMask :: Access
                                             , dstAccessMask :: Access
                                             , oldLayout :: ImageLayout
                                             , newLayout :: ImageLayout
                                             , srcQueueFamilyIndex :: Word
                                             , dstQueueFamilyIndex :: Word
                                             , image :: Image
                                             , subresourceRange :: ImageSubresourceRange
                                             }

type PipelineStage = Flags VkPipelineStageFlagBits

pattern TopOfPipe = Flags VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT :: PipelineStage
pattern DrawIndirect = Flags VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT :: PipelineStage
pattern VertexInput = Flags VK_PIPELINE_STAGE_VERTEX_INPUT_BIT :: PipelineStage
pattern VertexShader = Flags VK_PIPELINE_STAGE_VERTEX_SHADER_BIT :: PipelineStage
pattern TessellationControlShader = Flags VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT :: PipelineStage
pattern TessellationEvaluationShader = Flags VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT :: PipelineStage
pattern GeometryShader = Flags VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT :: PipelineStage
pattern FragmentShader = Flags VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT :: PipelineStage
pattern EarlyFragmentTests = Flags VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT :: PipelineStage
pattern LateFragmentTests = Flags VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT :: PipelineStage
pattern ColorAttachmentOutput = Flags VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT :: PipelineStage
pattern ComputeShader = Flags VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT :: PipelineStage
pattern Transfer = Flags VK_PIPELINE_STAGE_TRANSFER_BIT :: PipelineStage
pattern BottomOfPipe = Flags VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT :: PipelineStage
pattern Host = Flags VK_PIPELINE_STAGE_HOST_BIT :: PipelineStage
pattern AllGraphics = Flags VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT :: PipelineStage
pattern AllCommands = Flags VK_PIPELINE_STAGE_ALL_COMMANDS_BIT :: PipelineStage

type DependencyFlags = Flags VkDependencyFlags

data Offset2D = Offset2D Int32 Int32
              deriving (Eq, Ord, Show)

data Rect2D = Rect2D { offset :: Offset2D
                     , extent :: Extent2D
                     }
            deriving (Eq, Ord, Show)

data ClearColorValue = FloatColor (Vector 4 Float)
                       | IntColor (Vector 4 Int)
                       | UIntColor (Vector 4 Word)
                       deriving (Eq, Ord, Show)

data ClearDepthStencilValue = ClearDepthStencilValue { depth :: Float
                                                     , stencil :: Word
                                                     }
                            deriving (Eq, Ord, Show)

data ClearValue = ClearColor ClearColorValue
                  | ClearDepthStencil ClearDepthStencilValue
                  deriving (Eq, Ord, Show)

data RenderPassBeginInfo = RenderPassBeginInfo { renderPass :: RenderPass
                                               , framebuffer :: Framebuffer
                                               , renderArea :: Rect2D
                                               , clearValues :: [ClearValue]
                                               }

type SubpassContents = Enumerator VkSubpassContents

pattern Inline = Enumerator VK_SUBPASS_CONTENTS_INLINE :: SubpassContents

type DescriptorSet = Handle VkDescriptorSet

data SubmitInfo = SubmitInfo { waitSemaphores :: [(Semaphore, PipelineStage)]
                             , commandBuffers :: [CommandBuffer]
                             , signalSemaphores :: [Semaphore]
                             }

data PresentInfo = PresentInfo { waitSemaphores :: [Semaphore]
                               , swapchains :: [(Swapchain, Word)]
                               }

type PipelineCreateFlags = Flags VkPipelineCreateFlags

type PipelineShaderStageCreateFlags = Flags VkPipelineShaderStageCreateFlags

type ShaderStage = Flags VkShaderStageFlagBits

pattern Vertex = Flags VK_SHADER_STAGE_VERTEX_BIT :: ShaderStage
pattern TessellationControl = Flags VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT :: ShaderStage
pattern TessellationEvaluation = Flags VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT :: ShaderStage
pattern Geometry = Flags VK_SHADER_STAGE_GEOMETRY_BIT :: ShaderStage
pattern Fragment = Flags VK_SHADER_STAGE_FRAGMENT_BIT :: ShaderStage
pattern Compute = Flags VK_SHADER_STAGE_COMPUTE_BIT :: ShaderStage
pattern AllGraphicsShader = Flags VK_SHADER_STAGE_ALL_GRAPHICS :: ShaderStage
pattern All = Flags VK_SHADER_STAGE_ALL :: ShaderStage

type ShaderModule = Handle VkShaderModule

data SpecializationMapEntry = SpecializationMapEntry { constantID :: Word
                                                     , offset :: Word
                                                     , size :: CSize
                                                     }
                            deriving (Eq, Ord, Show)

data SpecializationInfo = SpecializationInfo { mapEntries :: [SpecializationMapEntry]
                                             , dataSize :: CSize
                                             , pdata :: Ptr Void
                                             }
                        deriving (Eq, Ord, Show)

data PipelineShaderStageCreateInfo = PipelineShaderStageCreateInfo { flags :: PipelineShaderStageCreateFlags
                                                                   , stage :: ShaderStage
                                                                   , shaderModule :: ShaderModule
                                                                   , name :: String
                                                                   , specializationInfo :: Maybe SpecializationInfo
                                                                   }

type PipelineVertexInputStateCreateFlags = Flags VkPipelineVertexInputStateCreateFlags

type VertexInputRate = Enumerator VkVertexInputRate

pattern PerVertex = Enumerator VK_VERTEX_INPUT_RATE_VERTEX :: VertexInputRate
pattern PerInstance = Enumerator VK_VERTEX_INPUT_RATE_INSTANCE :: VertexInputRate

data VertexInputBindingDescription = VertexInputBindingDescription { binding :: Word
                                                                   , stride :: Word
                                                                   , inputRate :: VertexInputRate
                                                                   }
                                   deriving (Eq, Ord, Show)

data VertexInputAttributeDescription = VertexInputAttributeDescription { location :: Word
                                                                       , binding :: Word
                                                                       , format :: Format
                                                                       , offset :: Word
                                                                       }
                                     deriving (Eq, Ord, Show)

data PipelineVertexInputStateCreateInfo = PipelineVertexInputStateCreateInfo
  { flags :: PipelineVertexInputStateCreateFlags
  , vertexBindingDescriptions :: [VertexInputBindingDescription]
  , vertexAttributeDescriptions :: [VertexInputAttributeDescription]
  }
  deriving (Eq, Ord, Show)

type PipelineInputAssemblyStateCreateFlags = Flags VkPipelineInputAssemblyStateCreateFlags

type PrimitiveTopology = Enumerator VkPrimitiveTopology

pattern PointList = Enumerator VK_PRIMITIVE_TOPOLOGY_POINT_LIST :: PrimitiveTopology
pattern LineList = Enumerator VK_PRIMITIVE_TOPOLOGY_LINE_LIST :: PrimitiveTopology
pattern LineStrip = Enumerator VK_PRIMITIVE_TOPOLOGY_LINE_STRIP :: PrimitiveTopology
pattern TriangleList = Enumerator VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST :: PrimitiveTopology
pattern TriangleStrip = Enumerator VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP :: PrimitiveTopology
pattern TriangleFan = Enumerator VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN :: PrimitiveTopology
pattern LineListWithAdjacency = Enumerator VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY :: PrimitiveTopology
pattern LineStripWithAdjacency = Enumerator VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY :: PrimitiveTopology
pattern TriangleListWithAdjacency = Enumerator VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY :: PrimitiveTopology
pattern TriangleStripWithAdjacency = Enumerator VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY :: PrimitiveTopology
pattern PatchList = Enumerator VK_PRIMITIVE_TOPOLOGY_PATCH_LIST :: PrimitiveTopology

data PipelineInputAssemblyStateCreateInfo = PipelineInputAssemblyStateCreateInfo
  { flags :: PipelineInputAssemblyStateCreateFlags
  , topology :: PrimitiveTopology
  , primitiveRestartEnable :: Bool
  }
  deriving (Eq, Ord, Show)

type PipelineTessellationStateCreateFlags = Flags VkPipelineTessellationStateCreateFlags

data PipelineTessellationStateCreateInfo = PipelineTessellationStateCreateInfo
  { flags :: PipelineTessellationStateCreateFlags
  , patchControlPoints :: Word
  }
  deriving (Eq, Ord, Show)

type PipelineViewportStateCreateFlags = Flags VkPipelineViewportStateCreateFlags

data Viewport = Viewport { x :: Float
                         , y :: Float
                         , width :: Float
                         , height :: Float
                         , minDepth :: Float
                         , maxDepth :: Float
                         }
              deriving (Eq, Ord, Show)

data PipelineViewportStateCreateInfo = PipelineViewportStateCreateInfo
                                   { flags :: PipelineViewportStateCreateFlags
                                   -- TODO Either Int [Viewport]
                                   , viewportCount :: Word
                                   , viewports :: Maybe [Viewport]
                                   , scissorCount :: Word
                                   , scissors :: Maybe [Rect2D]
                                   }
  deriving (Eq, Ord, Show)

type PipelineRasterizationStateCreateFlags = Flags VkPipelineRasterizationStateCreateFlags

type PolygonMode = Enumerator VkPolygonMode

pattern Fill = Enumerator VK_POLYGON_MODE_FILL :: PolygonMode
pattern Line = Enumerator VK_POLYGON_MODE_LINE :: PolygonMode
pattern Point = Enumerator VK_POLYGON_MODE_POINT :: PolygonMode

type CullMode = Flags VkCullModeFlags

pattern Front = Flags VK_CULL_MODE_FRONT_BIT :: CullMode
pattern Back = Flags VK_CULL_MODE_BACK_BIT :: CullMode
pattern None = Flags VK_CULL_MODE_NONE :: CullMode
pattern FrontAndBack = Flags VK_CULL_MODE_FRONT_AND_BACK :: CullMode

type FrontFace = Enumerator VkFrontFace

pattern CounterClockwise = Enumerator VK_FRONT_FACE_COUNTER_CLOCKWISE :: FrontFace
pattern Clockwise = Enumerator VK_FRONT_FACE_CLOCKWISE :: FrontFace

data PipelineRasterizationStateCreateInfo = PipelineRasterizationStateCreateInfo
                                            { flags :: PipelineRasterizationStateCreateFlags
                                            , depthClampEnable :: Bool
                                            , rasterizerDiscardEnable :: Bool
                                            , polygonMode :: PolygonMode
                                            , cullMode :: CullMode
                                            , frontFace :: FrontFace
                                            , depthBiasEnable :: Bool
                                            , depthBiasConstantFactor :: Float
                                            , depthBiasClamp :: Float
                                            , depthBiasSlopeFactor :: Float
                                            , lineWidth :: Float
                                            }
                                          deriving (Eq, Ord, Show)

type PipelineMultisampleStateCreateFlags = Flags VkPipelineMultisampleStateCreateFlags

newtype SampleMask = SampleMask Word
                   deriving (Eq, Ord, Show)

data PipelineMultisampleStateCreateInfo = PipelineMultisampleStateCreateInfo
                                          { flags :: PipelineMultisampleStateCreateFlags
                                          , rasterizationSamples :: SampleCountFlags
                                          , sampleShadingEnable :: Bool
                                          , minSampleShading :: Float
                                          , sampleMask :: Maybe [SampleMask]
                                          , alphaToCoverageEnable :: Bool
                                          , alphaToOneEnable :: Bool
                                          }
                                        deriving (Eq, Ord, Show)

type PipelineDepthStencilStateCreateFlags = Flags VkPipelineDepthStencilStateCreateFlags

type StencilOp = Enumerator VkStencilOp

pattern Keep = Enumerator VK_STENCIL_OP_KEEP :: StencilOp
pattern ZeroOp = Enumerator VK_STENCIL_OP_ZERO :: StencilOp
pattern Replace = Enumerator VK_STENCIL_OP_REPLACE :: StencilOp
pattern IncrementAndClamp = Enumerator VK_STENCIL_OP_INCREMENT_AND_CLAMP :: StencilOp
pattern DecrementAndClamp = Enumerator VK_STENCIL_OP_DECREMENT_AND_CLAMP :: StencilOp
pattern Invert = Enumerator VK_STENCIL_OP_INVERT :: StencilOp
pattern IncrementAndWrap = Enumerator VK_STENCIL_OP_INCREMENT_AND_WRAP :: StencilOp
pattern DecrementAndWrap = Enumerator VK_STENCIL_OP_DECREMENT_AND_WRAP :: StencilOp

type CompareOp = Enumerator VkCompareOp

pattern Never = Enumerator VK_COMPARE_OP_NEVER :: CompareOp
pattern Less = Enumerator VK_COMPARE_OP_LESS :: CompareOp
pattern Equal = Enumerator VK_COMPARE_OP_EQUAL :: CompareOp
pattern LessOrEqual = Enumerator VK_COMPARE_OP_LESS_OR_EQUAL :: CompareOp
pattern Greater = Enumerator VK_COMPARE_OP_GREATER :: CompareOp
pattern NotEqual = Enumerator VK_COMPARE_OP_NOT_EQUAL :: CompareOp
pattern GreaterOrEqual = Enumerator VK_COMPARE_OP_GREATER_OR_EQUAL :: CompareOp
pattern Always = Enumerator VK_COMPARE_OP_ALWAYS :: CompareOp

data StencilOpState = StencilOpState { failOp :: StencilOp
                                     , passOp :: StencilOp
                                     , depthFailOp :: StencilOp
                                     , compareOp :: CompareOp
                                     , compareMask :: Word
                                     , writeMask :: Word
                                     , reference :: Word
                                     }
                    deriving (Eq, Ord, Show)

data PipelineDepthStencilStateCreateInfo = PipelineDepthStencilStateCreateInfo
                                           { flags :: PipelineDepthStencilStateCreateFlags
                                           , depthTestEnable :: Bool
                                           , depthWriteEnable :: Bool
                                           , depthCompareOp :: CompareOp
                                           , depthBoundsTestEnable :: Bool
                                           , stencilTestEnable :: Bool
                                           , front :: StencilOpState
                                           , back :: StencilOpState
                                           , minDepthBounds :: Float
                                           , maxDepthBounds :: Float
                                           }
                                         deriving (Eq, Ord, Show)

type PipelineColorBlendStateCreateFlags = Flags VkPipelineColorBlendStateCreateFlags

type LogicOp = Enumerator VkLogicOp

pattern ClearOp = Enumerator VK_LOGIC_OP_CLEAR :: LogicOp
pattern And = Enumerator VK_LOGIC_OP_AND :: LogicOp
pattern AndReverse = Enumerator VK_LOGIC_OP_AND_REVERSE :: LogicOp
pattern Copy = Enumerator VK_LOGIC_OP_COPY :: LogicOp
pattern AndInverted = Enumerator VK_LOGIC_OP_AND_INVERTED :: LogicOp
pattern NoOp = Enumerator VK_LOGIC_OP_NO_OP :: LogicOp
pattern Xor = Enumerator VK_LOGIC_OP_XOR :: LogicOp
pattern Or = Enumerator VK_LOGIC_OP_OR :: LogicOp
pattern Nor = Enumerator VK_LOGIC_OP_NOR :: LogicOp
pattern Equivalent = Enumerator VK_LOGIC_OP_EQUIVALENT :: LogicOp
pattern InvertOp = Enumerator VK_LOGIC_OP_INVERT :: LogicOp
pattern OrReverse = Enumerator VK_LOGIC_OP_OR_REVERSE :: LogicOp
pattern CopyInverted = Enumerator VK_LOGIC_OP_COPY_INVERTED :: LogicOp
pattern OrInverted = Enumerator VK_LOGIC_OP_OR_INVERTED :: LogicOp
pattern Nand = Enumerator VK_LOGIC_OP_NAND :: LogicOp
pattern Set = Enumerator VK_LOGIC_OP_SET :: LogicOp

type BlendFactor = Enumerator VkBlendFactor

pattern Zero = Enumerator VK_BLEND_FACTOR_ZERO :: BlendFactor
pattern One = Enumerator VK_BLEND_FACTOR_ONE :: BlendFactor
pattern SrcColor = Enumerator VK_BLEND_FACTOR_SRC_COLOR :: BlendFactor
pattern OneMinusSrcColor = Enumerator VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR :: BlendFactor
pattern DstColor = Enumerator VK_BLEND_FACTOR_DST_COLOR :: BlendFactor
pattern OneMinusDstColor = Enumerator VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR :: BlendFactor
pattern SrcAlpha = Enumerator VK_BLEND_FACTOR_SRC_ALPHA :: BlendFactor
pattern OneMinusSrcAlpha = Enumerator VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA :: BlendFactor
pattern DstAlpha = Enumerator VK_BLEND_FACTOR_DST_ALPHA :: BlendFactor
pattern OneMinusDstAlpha = Enumerator VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA :: BlendFactor
pattern ConstantColor = Enumerator VK_BLEND_FACTOR_CONSTANT_COLOR :: BlendFactor
pattern OneMinusConstantColor = Enumerator VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR :: BlendFactor
pattern ConstantAlpha = Enumerator VK_BLEND_FACTOR_CONSTANT_ALPHA :: BlendFactor
pattern OneMinusConstantAlpha = Enumerator VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA :: BlendFactor
pattern SrcAlphaSaturate = Enumerator VK_BLEND_FACTOR_SRC_ALPHA_SATURATE :: BlendFactor
pattern SrC1Color = Enumerator VK_BLEND_FACTOR_SRC1_COLOR :: BlendFactor
pattern OneMinusSrC1Color = Enumerator VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR :: BlendFactor
pattern SrC1Alpha = Enumerator VK_BLEND_FACTOR_SRC1_ALPHA :: BlendFactor
pattern OneMinusSrC1Alpha = Enumerator VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA :: BlendFactor

type BlendOp = Enumerator VkBlendOp

pattern Add = Enumerator VK_BLEND_OP_ADD :: BlendOp
pattern Subtract = Enumerator VK_BLEND_OP_SUBTRACT :: BlendOp
pattern ReverseSubtract = Enumerator VK_BLEND_OP_REVERSE_SUBTRACT :: BlendOp
pattern Min = Enumerator VK_BLEND_OP_MIN :: BlendOp
pattern Max = Enumerator VK_BLEND_OP_MAX :: BlendOp

type ColorComponent = Flags VkColorComponentFlags

pattern Red = Flags VK_COLOR_COMPONENT_R_BIT :: ColorComponent
pattern Green = Flags VK_COLOR_COMPONENT_G_BIT :: ColorComponent
pattern Blue = Flags VK_COLOR_COMPONENT_B_BIT :: ColorComponent
pattern Alpha = Flags VK_COLOR_COMPONENT_A_BIT :: ColorComponent

all :: ColorComponent
all = Red .|. Green .|. Blue .|. Alpha

data PipelineColorBlendAttachmentState = PipelineColorBlendAttachmentState
                                         { blendEnable :: Bool
                                         , srcColorBlendFactor :: BlendFactor
                                         , dstColorBlendFactor :: BlendFactor
                                         , colorBlendOp :: BlendOp
                                         , srcAlphaBlendFactor :: BlendFactor
                                         , dstAlphaBlendFactor :: BlendFactor
                                         , alphaBlendOp :: BlendOp
                                         , colorWriteMask :: ColorComponent
                                         }
                                       deriving (Eq, Ord, Show)

data PipelineColorBlendStateCreateInfo = PipelineColorBlendStateCreateInfo
                                         { flags :: PipelineColorBlendStateCreateFlags
                                         , logicOpEnable :: Bool
                                         , logicOp :: LogicOp
                                         , attachments :: [PipelineColorBlendAttachmentState]
                                         -- TODO Vector
                                         , blendConstants :: Vector 4 CFloat
                                         }
                                       deriving (Eq, Ord, Show)

type PipelineDynamicStateCreateFlags = Flags VkPipelineDynamicStateCreateFlags

type DynamicState = Enumerator VkDynamicState

pattern DynamicViewport = Enumerator VK_DYNAMIC_STATE_VIEWPORT :: DynamicState
viewport :: DynamicState
viewport = DynamicViewport
pattern Scissor = Enumerator VK_DYNAMIC_STATE_SCISSOR :: DynamicState
pattern LineWidth = Enumerator VK_DYNAMIC_STATE_LINE_WIDTH :: DynamicState
pattern DepthBias = Enumerator VK_DYNAMIC_STATE_DEPTH_BIAS :: DynamicState
pattern BlendConstants = Enumerator VK_DYNAMIC_STATE_BLEND_CONSTANTS :: DynamicState
pattern DepthBounds = Enumerator VK_DYNAMIC_STATE_DEPTH_BOUNDS :: DynamicState
pattern StencilCompareMask = Enumerator VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK :: DynamicState
pattern StencilWriteMask = Enumerator VK_DYNAMIC_STATE_STENCIL_WRITE_MASK :: DynamicState
pattern StencilReference = Enumerator VK_DYNAMIC_STATE_STENCIL_REFERENCE :: DynamicState

data PipelineDynamicStateCreateInfo = PipelineDynamicStateCreateInfo
                                      { flags :: PipelineDynamicStateCreateFlags
                                      , dynamicStates :: [DynamicState]
                                      }
  deriving (Eq, Ord, Show)

type PipelineLayout = Handle VkPipelineLayout

data GraphicsPipelineCreateInfo = GraphicsPipelineCreateInfo { flags :: PipelineCreateFlags
                                                             , stages :: [PipelineShaderStageCreateInfo]
                                                             , vertexInputState :: PipelineVertexInputStateCreateInfo
                                                             , inputAssemblyState :: PipelineInputAssemblyStateCreateInfo
                                                             , tessellationState :: Maybe PipelineTessellationStateCreateInfo
                                                             -- TODO: Combine null on raster states
                                                             , viewportState :: Maybe PipelineViewportStateCreateInfo
                                                             , rasterizationState :: PipelineRasterizationStateCreateInfo
                                                             , multisampleState :: Maybe PipelineMultisampleStateCreateInfo
                                                             , depthStencilState :: PipelineDepthStencilStateCreateInfo
                                                             , colorBlendState :: PipelineColorBlendStateCreateInfo
                                                             , dynamicState :: Maybe PipelineDynamicStateCreateInfo
                                                             , layout :: PipelineLayout
                                                             , renderPass :: RenderPass
                                                             , subpass :: Word
                                                             -- TODO: Either or, depends on flags
                                                             , basePipelineHandle :: Pipeline
                                                             , basePipelineIndex :: Int
                                                             }

type Pipeline = Handle VkPipeline

type PipelineLayoutCreateFlags = Flags VkPipelineLayoutCreateFlags

type DescriptorSetLayoutCreateFlags = Flags VkDescriptorSetLayoutCreateFlags

type DescriptorType = Enumerator VkDescriptorType

pattern Sampler = Enumerator VK_DESCRIPTOR_TYPE_SAMPLER :: DescriptorType
pattern CombinedImageSampler = Enumerator VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER :: DescriptorType
pattern SampledImage = Enumerator VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE :: DescriptorType
pattern StorageImage = Enumerator VK_DESCRIPTOR_TYPE_STORAGE_IMAGE :: DescriptorType
pattern UniformTexelBuffer = Enumerator VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER :: DescriptorType
pattern UniformBuffer = Enumerator VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER :: DescriptorType
pattern StorageBuffer = Enumerator VK_DESCRIPTOR_TYPE_STORAGE_BUFFER :: DescriptorType
pattern UniformBufferDynamic = Enumerator VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC :: DescriptorType
pattern StorageBufferDynamic = Enumerator VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC :: DescriptorType
pattern InputAttachment = Enumerator VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT :: DescriptorType

type Sampler = Handle VkSampler

data DescriptorSetLayoutBinding = DescriptorSetLayoutBinding
  { binding :: Word
  , descriptorType :: DescriptorType
  , descriptorCount :: Word
  , stageFlags :: ShaderStage
  , immutableSamplers :: Maybe [Sampler]
  }
  deriving (Eq, Ord, Show)

data DescriptorSetLayoutCreateInfo = DescriptorSetLayoutCreateInfo
  { flags :: DescriptorSetLayoutCreateFlags
  , bindings :: [DescriptorSetLayoutBinding]
  }
  deriving (Eq, Ord, Show)

type DescriptorSetLayout = Handle VkDescriptorSetLayout

data PushConstantRange = PushConstantRange
  { stageFlags :: ShaderStage
  , offset :: Word
  , size :: Word
  }
  deriving (Eq, Ord, Show)

data PipelineLayoutCreateInfo = PipelineLayoutCreateInfo
  { flags :: PipelineLayoutCreateFlags
  , setLayouts :: [DescriptorSetLayout]
  , pushConstantRanges :: [PushConstantRange]
  }
  deriving (Eq, Ord, Show)

type ShaderModuleCreateFlags = Flags VkShaderModuleCreateFlags

data ShaderModuleCreateInfo = ShaderModuleCreateInfo
  { flags :: ShaderModuleCreateFlags
  , code :: ByteString
  }
  deriving (Eq, Ord, Show)

type BufferCreateFlags = Flags VkBufferCreateFlags

type BufferUsage = Flags VkBufferUsageFlags

pattern TransferSrc = Flags VK_BUFFER_USAGE_TRANSFER_SRC_BIT :: BufferUsage
pattern TransferDst = Flags VK_BUFFER_USAGE_TRANSFER_DST_BIT :: BufferUsage
-- pattern UniformTexelBuffer = Flags VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT :: BufferUsage
pattern StorageTexelBuffer = Flags VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT :: BufferUsage
-- pattern UniformBuffer = Flags VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT :: BufferUsage
-- pattern StorageBuffer = Flags VK_BUFFER_USAGE_STORAGE_BUFFER_BIT :: BufferUsage
pattern IndexBuffer = Flags VK_BUFFER_USAGE_INDEX_BUFFER_BIT :: BufferUsage
pattern VertexBuffer = Flags VK_BUFFER_USAGE_VERTEX_BUFFER_BIT :: BufferUsage
pattern IndirectBuffer = Flags VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT :: BufferUsage

data BufferCreateInfo = BufferCreateInfo
  { flags :: BufferCreateFlags
  , size :: DeviceSize
  , usage :: BufferUsage
  , sharingMode :: SharingMode
  , queueFamilyIndices :: [Word]
  }
  deriving (Eq, Ord, Show)

data MemoryRequirements = MemoryRequirements
  { size :: DeviceSize
  , alignment :: DeviceSize
  , memoryTypeBits :: Word
  }
  deriving (Eq, Ord, Show)

type MemoryProperty = Flags VkMemoryPropertyFlags

pattern DeviceLocal = Flags VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT :: MemoryProperty
pattern HostVisible = Flags VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT :: MemoryProperty
pattern HostCoherent = Flags VK_MEMORY_PROPERTY_HOST_COHERENT_BIT :: MemoryProperty
pattern HostCached = Flags VK_MEMORY_PROPERTY_HOST_CACHED_BIT :: MemoryProperty
pattern LazilyAllocated = Flags VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT :: MemoryProperty

data MemoryType = MemoryType
  { propertyFlags :: MemoryProperty
  , heapIndex :: Word
  }
  deriving (Eq, Ord, Show)

type MemoryHeapFlags = Flags VkMemoryHeapFlags

data MemoryHeap = MemoryHeap
  { size :: DeviceSize
  , flags :: MemoryHeapFlags
  }
  deriving (Eq, Ord, Show)

data PhysicalDeviceMemoryProperties = PhysicalDeviceMemoryProperties
  { memoryType :: [MemoryType]
  , memoryHeaps :: [MemoryHeap]
  }
  deriving (Eq, Ord, Show)

type DeviceMemory = Handle VkDeviceMemory

data MemoryAllocateInfo = MemoryAllocateInfo
  { allocationSize :: DeviceSize
  , memoryTypeIndex :: Word
  }
  deriving (Eq, Ord, Show)

type MemoryMapFlags = Flags VkMemoryMapFlags

type DescriptorPool = Handle VkDescriptorPool

type DescriptorPoolCreateFlags = Flags VkDescriptorPoolCreateFlags

data DescriptorPoolSize = DescriptorPoolSize
  { descriptorType :: DescriptorType
  , descriptorCount :: Word
  }
  deriving (Eq, Ord, Show)

data DescriptorPoolCreateInfo = DescriptorPoolCreateInfo
  { flags :: DescriptorPoolCreateFlags
  , maxSets :: Word
  , poolSizes :: [DescriptorPoolSize]
  }
  deriving (Eq, Ord, Show)

data DescriptorSetAllocateInfo = DescriptorSetAllocateInfo
  { descriptorPool :: DescriptorPool
  , setLayouts :: [DescriptorSetLayout]
  }
  deriving (Eq, Ord, Show)

data DescriptorImageInfo = DescriptorImageInfo
  { sampler :: Sampler
  , imageView :: ImageView
  , imageLayout :: ImageLayout
  }
  deriving (Eq, Ord, Show)

data DescriptorBufferInfo = DescriptorBufferInfo
  { buffer :: Buffer
  , offset :: DeviceSize
  , range :: DeviceSize
  }
  deriving (Eq, Ord, Show)

type BufferView = Handle VkBufferView

data WriteDescriptorSet = WriteDescriptorSet
  { dstSet :: DescriptorSet
  , dstBinding :: Word
  , dstArrayElement :: Word
  , descriptorCount :: Word
  , descriptorType :: DescriptorType
  , imageInfo :: Maybe [DescriptorImageInfo]
  , bufferInfo :: Maybe [DescriptorBufferInfo]
  , texelBufferView :: Maybe [BufferView]
  }
  deriving (Eq, Ord, Show)

data CopyDescriptorSet = CopyDescriptorSet
  { srcSet :: DescriptorSet
  , srcBinding :: Word
  , srcArrayElement :: Word
  , dstSet :: DescriptorSet
  , dstBinding :: Word
  , dstArrayElement :: Word
  , descriptorCount :: Word
  }
  deriving (Eq, Ord, Show)

type SamplerCreateFlags = Flags VkSamplerCreateFlags

type Filter = Enumerator VkFilter
pattern NearestFilter = Enumerator VK_FILTER_NEAREST :: Filter
pattern LinearFilter = Enumerator VK_FILTER_LINEAR :: Filter

type SamplerMipmapMode = Enumerator VkSamplerMipmapMode
pattern NearestMip = Enumerator VK_SAMPLER_MIPMAP_MODE_NEAREST :: SamplerMipmapMode
pattern LinearMip = Enumerator VK_SAMPLER_MIPMAP_MODE_LINEAR :: SamplerMipmapMode

type SamplerAddressMode = Enumerator VkSamplerAddressMode
pattern Repeat = Enumerator VK_SAMPLER_ADDRESS_MODE_REPEAT :: SamplerAddressMode
pattern MirroredRepeat = Enumerator VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT :: SamplerAddressMode
pattern ClampToEdge = Enumerator VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE :: SamplerAddressMode
pattern ClampToBorder = Enumerator VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER :: SamplerAddressMode

type BorderColor = Enumerator VkBorderColor
pattern FloatTransparentBlack = Enumerator VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK :: BorderColor
pattern IntTransparentBlack = Enumerator VK_BORDER_COLOR_INT_TRANSPARENT_BLACK :: BorderColor
pattern FloatOpaqueBlack = Enumerator VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK :: BorderColor
pattern IntOpaqueBlack = Enumerator VK_BORDER_COLOR_INT_OPAQUE_BLACK :: BorderColor
pattern FloatOpaqueWhite = Enumerator VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE :: BorderColor
pattern IntOpaqueWhite = Enumerator VK_BORDER_COLOR_INT_OPAQUE_WHITE :: BorderColor

data SamplerCreateInfo = SamplerCreateInfo
  { flags :: SamplerCreateFlags
  , magFilter :: Filter
  , minFilter :: Filter
  , mipmapMode :: SamplerMipmapMode
  , addressModeU :: SamplerAddressMode
  , addressModeV :: SamplerAddressMode
  , addressModeW :: SamplerAddressMode
  , mipLodBias :: Float
  , anisotropyEnable :: Bool
  , maxAnisotropy :: Float
  , compareEnable :: Bool
  , compareOp :: CompareOp
  , minLod :: Float
  , maxLod :: Float
  , borderColor :: BorderColor
  , unnormalizedCoordinates :: Bool
  }
  deriving (Eq, Ord, Show)
