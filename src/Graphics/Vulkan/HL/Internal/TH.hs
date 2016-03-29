{-# language TemplateHaskell #-}

module Graphics.Vulkan.HL.Internal.TH where

import Data.Char
import Language.Haskell.TH

mkEnum :: String -> [String] -> DecsQ
mkEnum name enums = do
  em <- mapM enumMap enums
  let (_, cons) = unzip em
  Just targetType <- lookupTypeName $ "Vk" ++ name
  return
    [ DataD [] typeName [] Nothing (toCons <$> cons) (ConT <$> [''Eq, ''Ord, ''Show, ''Read])
    , SigD marshal (AppT (AppT ArrowT (ConT typeName)) (ConT targetType))
    , FunD marshal (marshalClause <$> em)
    , SigD unmarshal (AppT (AppT ArrowT (ConT targetType)) (ConT typeName))
    , FunD unmarshal (unmarhsalClause <$> em)
    ]
  where
    toCons cons = NormalC cons []
    typeName = mkName name
    marshal = mkName $ "marshal" ++ name
    unmarshal = mkName $ "unmarshal" ++ name
    targetPrefix = "VK_" ++ (toUpper <$> name) ++ "_"
    marshalClause (t, c) = Clause [ConP c []] (NormalB (ConE t)) []
    unmarhsalClause (t, c) = marshalClause (c, t)
    enumMap enum = do
      Just target <- lookupValueName $ targetPrefix ++ enum
      return (target, mkName $ filter (/= '_') enum)
