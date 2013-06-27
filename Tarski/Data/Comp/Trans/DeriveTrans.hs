module Tarski.Data.Comp.Trans.DeriveTrans (
    deriveTrans
  ) where

import Language.Haskell.TH

import Tarski.Data.Comp.Trans.Names ( baseTypes, smartConstrName, nameLab, simplifyDataInf )

deriveTrans :: Name -> [Name] -> Type -> Q [Dec]
deriveTrans root names term = do let classNm = mkName "Trans"
                                 funNm <- newName "trans"

                                 classDec <- mkClass classNm funNm term
                                 funDec <- mkFunc root funNm term
                                 instances <- mapM (mkInstance classNm funNm) names

                                 return $ [classDec] ++ funDec ++ instances

{-
   Really wanted to use quasiquotes, but could not antiquote name in signature

  Example:
  translate :: J.CompilationUnit -> JavaTerm CompilationUnitL
  translate = trans
-}
mkFunc :: Name -> Name -> Type -> Q [Dec]
mkFunc typ funNm term = return [ SigD translate (AppT (AppT ArrowT (ConT typ)) (AppT term lab))
                               , ValD (VarP translate) (NormalB funNm') []
                               ]
  where
    translate = mkName "translate"
    lab = ConT $ nameLab typ
    funNm' = VarE funNm

{-
  class Trans a l where
    trans a -> JavaTerm l
-}
mkClass :: Name -> Name -> Type -> Q Dec
mkClass classNm funNm term = do a <- newName "a"
                                i <- newName "i"
                                let transDec = SigD funNm (foldl AppT ArrowT [VarT a, AppT term (VarT i)])
                                return $ ClassD [] classNm [PlainTV a, PlainTV i] [] [transDec]

{-
  instance Trans J.CompilationUnit CompilationUnitL where
    trans (J.CompilationUnit x y z) = iCompilationUnit (trans x) (trans y) (trans z)
-}
mkInstance :: Name -> Name -> Name -> Q Dec
mkInstance classNm funNm typNm = do inf <- reify typNm
                                    nmTyps <- simplifyDataInf inf
                                    clauses <- mapM (uncurry $ mkClause funNm) nmTyps
                                    let targNm = nameLab typNm
                                    return (InstanceD []
                                                      (AppT (AppT (ConT classNm) (ConT typNm)) (ConT targNm))
                                                      [FunD funNm clauses])

mkClause :: Name -> Name -> [Type] -> Q Clause
mkClause funNm con tps = do nms <- mapM (const $ newName "x") tps
                            return $ Clause [pat nms] (body nms) []
  where
    pat nms = ConP con (map VarP nms)

    body nms = NormalB $ foldl AppE (VarE (smartConstrName con)) (map atom $ zip nms tps)

    atom :: (Name, Type) -> Exp
    atom (x, t) | elem t baseTypes = VarE x
    atom (x, _)                    = AppE (VarE funNm) (VarE x)