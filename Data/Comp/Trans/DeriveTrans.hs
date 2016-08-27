module Data.Comp.Trans.DeriveTrans
  (
    deriveTrans
  ) where

import Control.Monad.Trans ( lift )

import Language.Haskell.TH

import Data.Comp.Trans.Util

-- |
-- Creates a functions translating from an ADT
-- to its isomorphic multi-sorted compositional data type
-- 
-- @
-- import qualified Foo as F
-- ...
-- type ArithTerm = Term Arith
-- runCompTrans $ deriveTrans ''Arith [''Arith, ''Atom, ''Lit] ArithTerm
-- @
-- 
-- will create
-- 
-- @
-- translate :: F.Arith -> ArithTerm ArithL
-- translate = trans
-- 
-- 
-- class Trans a l where
--   trans a -> ArithTerm l
-- 
-- instance Trans F.Arith ArithL where
--   trans (F.Add x y) = iAdd (trans x) (trans y)
-- 
-- instance Trans F.Atom AtomL where
--   trans (F.Var s)   = iVar s
--   trans (F.Const x) = iConst (trans x)
-- 
-- instance Trans F.Lit LitL where
--   trans (F.Lit n) = iLit n
-- @
deriveTrans :: Name -> [Name] -> Type -> CompTrans [Dec]
deriveTrans root names term = do let classNm = mkName "Trans"
                                 funNm <- lift $ newName "trans"

                                 classDec <- mkClass classNm funNm term
                                 funDec <- mkFunc root funNm term
                                 instances <- mapM (mkInstance classNm funNm) names

                                 return $ [classDec] ++ funDec ++ instances

-- |
-- Example:
-- 
-- @
-- translate :: J.CompilationUnit -> JavaTerm CompilationUnitL
-- translate = trans
-- @
mkFunc :: Name -> Name -> Type -> CompTrans [Dec]
mkFunc typ funNm term = do
  srcTyp <- getFullyAppliedType typ
  return [ SigD translate (AppT (AppT ArrowT srcTyp) (AppT term lab))
         , ValD (VarP translate) (NormalB funNm') []
         ]
  where
    translate = mkName "translate"
    lab = ConT $ nameLab typ
    funNm' = VarE funNm

-- |
-- Example:
-- 
-- @
-- class Trans a l where
--   trans a -> JavaTerm l
-- @
mkClass :: Name -> Name -> Type -> CompTrans Dec
mkClass classNm funNm term = do a <- lift $ newName "a"
                                i <- lift $ newName "i"
                                let transDec = SigD funNm (foldl AppT ArrowT [VarT a, AppT term (VarT i)])
                                return $ ClassD [] classNm [PlainTV a, PlainTV i] [] [transDec]

-- |
-- Example:
-- 
-- @
-- instance Trans J.CompilationUnit CompilationUnitL where
--   trans (J.CompilationUnit x y z) = iCompilationUnit (trans x) (trans y) (trans z)
-- @
mkInstance :: Name -> Name -> Name -> CompTrans Dec
mkInstance classNm funNm typNm = do inf <- lift $ reify typNm
                                    srcTyp <- getFullyAppliedType typNm
                                    let nmTyps = simplifyDataInf inf
                                    clauses <- mapM (uncurry $ mkClause funNm) nmTyps
                                    let targNm = nameLab typNm
                                    return (InstanceD []
                                                      (AppT (AppT (ConT classNm) srcTyp) (ConT targNm))
                                                      [FunD funNm clauses])

mkClause :: Name -> Name -> [Type] -> CompTrans Clause
mkClause funNm con tps = do nms <- lift $ mapM (const $ newName "x") tps
                            return $ Clause [pat nms] (body nms) []
  where
    pat nms = ConP con (map VarP nms)

    body nms = NormalB $ foldl AppE (VarE (smartConstrName con)) (map atom $ zip nms tps)

    atom :: (Name, Type) -> Exp
    atom (x, t) | elem t baseTypes = VarE x
    atom (x, _)                    = AppE (VarE funNm) (VarE x)
