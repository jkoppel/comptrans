module Tarski.Data.Comp.Trans.DeriveMulti (
    deriveMulti
  ) where

import Control.Lens ( traverse, _1, _2, _3, (&), (%~), (%%~) )
import Control.Monad ( liftM )

import Data.Functor ( (<$>) )

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.ExpandSyns

deriveMulti :: Name -> Q [Dec]
deriveMulti n = do inf <- reify n
                   case inf of
                     TyConI (DataD _ nm [] cons _)   -> mkGADT nm cons
                     TyConI (NewtypeD _ nm [] con _) -> mkGADT nm [con]
                     _                         -> do reportError $ "Attempted to derive multi-sorted compositional data type for "
                                                                    ++ show n ++ ", which is not a nullary datatype"
                                                     return []

mkGADT :: Name -> [Con] -> Q [Dec]
mkGADT n cons = do e <- newName "e"
                   i <- newName "i"
                   let n' = mkName $ nameBase n
                   cons' <- mapM (mkCon n' e i) cons
                   return $ [DataD [] n' [PlainTV e, PlainTV i] cons' []
                            ,DataD [] (nameLab n) [] [] []
                            ]

mkCon :: Name -> Name -> Name -> Con -> Q Con
mkCon l e i (NormalC n sts) = ForallC [] ctx <$> inner
  where
    ctx = [EqualP (VarT i) (ConT $ nameLab l)]

    sts'  = sts & (traverse._2) %%~ unfixType e
    inner = liftM (NormalC (mkName $ nameBase n)) sts'
mkCon l e i (RecC n vsts) = ForallC [] ctx <$> inner
  where
    ctx = [EqualP (VarT i) (ConT $ nameLab l)]

    vsts'  = vsts & (traverse._1) %~ (mkName.nameBase)
    vsts'' = vsts' & (traverse._3) %%~ unfixType e
    inner  = liftM (RecC (mkName $ nameBase n)) vsts''
mkCon _ _ _ c = fail $ "Attempted to derive multi-sorted compositional datatype for something with non-normal constructors: " ++ show c

baseTypes :: [Type]
baseTypes = [ConT ''Int
            ,ConT ''Bool
            ,ConT ''Char
            ,ConT ''Double
            ,ConT ''Integer
            ,ConT ''String
            ,AppT ListT (ConT ''Char)
            ]

unfixType :: Name -> Type -> Q Type
unfixType _ t | elem t baseTypes = return t
unfixType e t = do t' <- expandSyns t >>= getLab
                   return $ AppT (VarT e) t'

getLab :: Type -> Q Type
getLab (AppT f t) = AppT f <$> getLab t
getLab ListT      = return ListT
getLab (TupleT n) = return $ TupleT n
getLab (ConT n)   = return $ ConT $ nameLab n
getLab _          = fail "When deriving multi-sorted compositional data type, found unsupported type in AST."

nameLab :: Name -> Name
nameLab n = mkName $ nameBase n ++ "L"