{-# language TemplateHaskell #-}

module Graphics.Vulkan.HL.Internal.TH where

import Data.Char
import Language.Haskell.TH

mkEnum :: String -> [String] -> DecsQ
mkEnum name enums = do
  em <- mapM enumMap enums
  let (_, cons) = unzip em
  targetType <- check lookupTypeName $ "Vk" ++ name
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
      target <- check lookupValueName $ targetPrefix ++ enum
      return (target, mkName $ filter (/= '_') enum)
    check l n = do
      tn <- l n
      case tn of
        Just tn' -> return tn'
        Nothing -> error $ "Unable to find: " ++ n
