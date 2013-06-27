module Tarski.Data.Comp.Trans.Names (
    standardNameSet
  , baseTypes
  , getLab
  , transName
  , nameLab
  , smartConstrName
  , modNameBase
  , simplifyDataInf
  ) where

import Control.Lens ( (^.), _3 )
import Control.Monad ( liftM2 )

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


getLab :: Type -> Q Type
getLab (AppT f@(AppT _ _) t) = liftM2 AppT (getLab f) (getLab t)
getLab (AppT f t) = AppT f <$> getLab t
getLab ListT      = return ListT
getLab (TupleT n) = return $ TupleT n
getLab (ConT n)   = return $ ConT $ nameLab n
getLab _          = fail "When deriving multi-sorted compositional data type, found unsupported type in AST."


transName :: Name -> Name
transName = modNameBase id

nameLab :: Name -> Name
nameLab = modNameBase (++"L")

smartConstrName :: Name -> Name
smartConstrName = modNameBase ('i':)

modNameBase :: (String -> String) -> Name -> Name
modNameBase f = mkName . f . nameBase

simplifyDataInf :: Info -> Q [(Name, [Type])]
simplifyDataInf (TyConI (DataD _ _ [] cons _))   = mapM extractCon cons
simplifyDataInf (TyConI (NewtypeD _ _ [] con _)) = sequence [extractCon con]
simplifyDataInf _                                = fail $ "Attempted to derive multi-sorted compositional data type for non-nullary datatype"

extractCon :: Con -> Q (Name, [Type])
extractCon (NormalC nm sts) = return (nm, map snd sts)
extractCon (RecC nm vsts)   = return (nm, map (^. _3) vsts)
extractCon _                = fail "Unsupported constructor type encountered"