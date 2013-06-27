module Tarski.Data.Comp.Trans.Names (
    standardNameSet,
    baseTypes,
    transName,
    getLab,
    nameLab,
    smartConstrName,
    modNameBase
  ) where

import Data.Functor ( (<$>) )
import Data.Set ( Set, fromList )

import Language.Haskell.TH.Syntax

{-
   Names that should be excluded from an AST hierarchy.

   Type synonyms need not be present.
-}
standardNameSet :: Set Name
standardNameSet = fromList [''Maybe, ''Int, ''Integer, ''Bool, ''Char, ''Double]


{-
   Types which should be translated into functorial form.
  
   Both String and its expansion are present because
   expandSyn threw errors
 -}
baseTypes :: [Type]
baseTypes = [ ConT ''Int
            , ConT ''Bool
            , ConT ''Char
            , ConT ''Double
            , ConT ''Integer
            , ConT ''String
            , AppT ListT (ConT ''Char)
            ]

transName :: Name -> Name
transName = mkName . nameBase


getLab :: Type -> Q Type
getLab (AppT f t) = AppT f <$> getLab t
getLab ListT      = return ListT
getLab (TupleT n) = return $ TupleT n
getLab (ConT n)   = return $ ConT $ nameLab n
getLab _          = fail "When deriving multi-sorted compositional data type, found unsupported type in AST."

nameLab :: Name -> Name
nameLab = modNameBase (++"L")

smartConstrName :: Name -> Name
smartConstrName = modNameBase ('i':)

modNameBase :: (String -> String) -> Name -> Name
modNameBase f = mkName . f . nameBase