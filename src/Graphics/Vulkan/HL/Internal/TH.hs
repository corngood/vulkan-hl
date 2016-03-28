{-# language TemplateHaskell #-}

module Graphics.Vulkan.HL.Internal.TH where

import Graphics.Vulkan.Core
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

formats :: Name -> DecsQ
formats n = do
  t <- reify n
  mod <- reifyModule (Module (PkgName "vulkan") (ModName "Graphics.Vulkan.Core"))
  return $ [ValD (VarP (mkName "x")) (NormalB (LitE (StringL (show mod)))) []]
