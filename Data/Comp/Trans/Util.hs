{-# LANGUAGE TemplateHaskell #-}

module Data.Comp.Trans.Util
  (
    TransCtx(..)
  , allTypes
  , substitutions
  , emptyTransCtx
  , withAllTypes
  , withSubstitutions
    
  , CompTrans
  , runCompTrans
    
  , standardNameSet
  , baseTypes
  , getLab
  , transName
  , nameLab
  , smartConstrName
  , modNameBase
  , simplifyDataInf
  , getTypeArgs
  , getNames
  , containsAll
  , getFullyAppliedType
  , applySubsts
  ) where

import Control.Lens ( (^.), (.~), _3, makeClassy, view )
import Control.Monad ( liftM2 )
import Control.Monad.Reader ( ReaderT(..), local )
import Control.Monad.Trans ( lift )

import Data.Data ( Data )
import Data.Generics ( everywhere, mkT )
import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Set ( Set, fromList )

import Language.Haskell.TH.Syntax hiding ( lift )


type CompTrans = ReaderT TransCtx Q
data TransCtx = TransCtx {
                           _allTypes      :: [Name]
                         , _substitutions :: Map.Map Name Type
                         }
                
makeClassy ''TransCtx

emptyTransCtx :: TransCtx
emptyTransCtx = TransCtx {
                           _allTypes      = []
                         , _substitutions = Map.empty
                         }

runCompTrans :: CompTrans a -> Q a
runCompTrans m = runReaderT m emptyTransCtx

withSubstitutions :: Map.Map Name Type -> CompTrans a -> CompTrans a
withSubstitutions substs = local (substitutions .~ substs)

withAllTypes :: [Name] -> CompTrans a -> CompTrans a
withAllTypes names = local (allTypes .~ names)

{-
   Names that should be excluded from an AST hierarchy.

   Type synonyms need not be present.
-}
standardNameSet :: Set Name
standardNameSet = fromList [''Maybe, ''Either, ''Int, ''Integer, ''Bool, ''Char, ''Double]


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


getLab :: Type -> CompTrans Type
getLab (AppT f@(AppT _ _) t) = liftM2 AppT (getLab f) (getLab t)
getLab (AppT (ConT n) t) = do
  names <- view allTypes
  if elem n names then
    return $ ConT $ nameLab n
   else
    AppT (ConT n) <$> getLab t
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

simplifyDataInf :: Info -> [(Name, [Type])]
simplifyDataInf (TyConI (DataD _ _ _ cons _))   = map extractCon cons
simplifyDataInf (TyConI (NewtypeD _ _ _ con _)) = [extractCon con]
simplifyDataInf _                               = error "Attempted to derive multi-sorted compositional data type for non-nullary datatype"

extractCon :: Con -> (Name, [Type])
extractCon (NormalC nm sts) = (nm, map snd sts)
extractCon (RecC nm vsts)   = (nm, map (^. _3) vsts)
extractCon (ForallC _ _ c)  = extractCon c
extractCon _                = error "Unsupported constructor type encountered"

getTypeArgs :: Name -> CompTrans [Name]
getTypeArgs nm = do
  inf <- lift $ reify nm
  case inf of
    TyConI (DataD _ _ tvs _ _)    -> return $ getNames tvs
    TyConI (NewtypeD _ _ tvs _ _) -> return $ getNames tvs
    _                             -> return []

getNames :: [TyVarBndr] -> [Name]
getNames = map getName
  where
    getName :: TyVarBndr -> Name
    getName (PlainTV n)    = n
    getName (KindedTV n _) = n

containsAll :: (Ord a) => Map a b -> [a] -> Bool
containsAll mp = all (`Map.member` mp)

getFullyAppliedType :: Name -> CompTrans Type
getFullyAppliedType nm = do
  substs <- view substitutions
  typeArgs <- getTypeArgs nm
  return $ foldl AppT (ConT nm) (applySubsts substs $ map VarT typeArgs)

applySubsts :: (Data x) => Map Name Type -> x -> x
applySubsts mp = everywhere (mkT subst1)
  where
    subst1 :: Type -> Type
    subst1 t@(VarT n) = case Map.lookup n mp of
      Just res -> res
      Nothing  -> t
    subst1 t          = t
