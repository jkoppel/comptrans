module Data.Comp.Trans.DeriveMulti (
    deriveMulti
  ) where

import Control.Lens ( traverse, _1, _2, _3, (&), (%~), (%%~) )
import Control.Monad ( liftM )

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.ExpandSyns ( expandSyns )

import Data.Comp.Trans.Names ( baseTypes, transName, nameLab, getLab )

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
                   let n' = transName n
                   cons' <- mapM (mkCon n' e i) cons
                   return $ [DataD [] n' [KindedTV e (AppT (AppT ArrowT StarT) StarT), PlainTV i] cons' []
                            ,DataD [] (nameLab n) [] [] []
                            ]

mkCon :: Name -> Name -> Name -> Con -> Q Con
mkCon l e i (NormalC n sts) = ForallC [] ctx <$> inner
  where
    ctx = [foldl AppT EqualityT [(VarT i), (ConT $ nameLab l)]]

    sts'  = sts & (traverse._2) %%~ unfixType e
    inner = liftM (NormalC (transName n)) sts'
mkCon l e i (RecC n vsts) = ForallC [] ctx <$> inner
  where
    ctx = [foldl AppT EqualityT [(VarT i), (ConT $ nameLab l)]]

    vsts'  = vsts & (traverse._1) %~ transName
    vsts'' = vsts' & (traverse._3) %%~ unfixType e
    inner  = liftM (RecC (transName n)) vsts''
mkCon _ _ _ c = fail $ "Attempted to derive multi-sorted compositional datatype for something with non-normal constructors: " ++ show c

unfixType :: Name -> Type -> Q Type
unfixType _ t | elem t baseTypes = return t
unfixType e t = do t' <- expandSyns t >>= getLab
                   return $ AppT (VarT e) t'
