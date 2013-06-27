module Tarski.Data.Comp.Trans.DeriveTrans (
    deriveTrans
  ) where

import Control.Lens ( (^.), _3 )

import Language.Haskell.TH

import Tarski.Data.Comp.Trans.Names ( baseTypes, smartConstrName, nameLab )

deriveTrans :: Name -> [Name] -> Type -> Q [Dec]
deriveTrans root names term = do let classNm = mkName "Trans"
                                 funNm <- newName "trans"

                                 classDec <- mkClass classNm funNm term
                                 funDec <- mkFunc root funNm term
                                 instances <- mapM (mkInstance classNm funNm) names

                                 return $ [classDec] ++ funDec ++ instances

{-
   Really wanted to use quasiquotes, but could not antiquote name in signature
-}
mkFunc :: Name -> Name -> Type -> Q [Dec]
mkFunc typ funNm term = return [ SigD translate (AppT (AppT ArrowT (VarT typ)) (AppT term lab))
                               , ValD (VarP translate) (NormalB funNm') []
                               ]
  where
    translate = mkName "translate"
    lab = VarT $ nameLab typ
    funNm' = VarE funNm

mkClass :: Name -> Name -> Type -> Q Dec
mkClass classNm funNm term = do a <- newName "a"
                                i <- newName "i"
                                let transDec = SigD funNm (foldl AppT ArrowT [VarT a, AppT term (VarT i)])
                                return $ ClassD [] classNm [PlainTV a, PlainTV i] [] [transDec]

mkInstance :: Name -> Name -> Name -> Q Dec
mkInstance classNm funNm typNm = do inf <- reify typNm
                                    nmTyps <- extract inf
                                    clauses <- mapM (uncurry $ mkClause funNm) nmTyps
                                    let targNm = nameLab typNm
                                    return (InstanceD []
                                                      (AppT (AppT (ConT classNm) (ConT typNm)) (ConT targNm))
                                                      [FunD funNm clauses])
  where
    extract :: Info -> Q [(Name, [Type])]
    extract (TyConI (DataD _ _ [] cons _))   = mapM extractCon cons
    extract (TyConI (NewtypeD _ _ [] con _)) = sequence [extractCon con]
    extract _                                 = fail $ "Attempted to derive multi-sorted compositional data type for "
                                                       ++ show typNm ++ ", which is not a nullary datatype"
    extractCon :: Con -> Q (Name, [Type])
    extractCon (NormalC nm sts) = return (nm, map snd sts)
    extractCon (RecC nm vsts)   = return (nm, map (^. _3) vsts)
    extractCon _                = fail "Unsupported constructor type encountered"
    

mkClause :: Name -> Name -> [Type] -> Q Clause
mkClause funNm con tps = do nms <- mapM (const $ newName "x") tps
                            return $ Clause [pat nms] (body nms) []
  where
    pat nms = ConP con (map VarP nms)

    body nms = NormalB $ foldl AppE (ConE (smartConstrName con)) (map atom $ zip nms tps)

    atom :: (Name, Type) -> Exp
    atom (x, t) | elem t baseTypes = VarE x
    atom (x, _)                    = AppE (VarE funNm) (VarE x)