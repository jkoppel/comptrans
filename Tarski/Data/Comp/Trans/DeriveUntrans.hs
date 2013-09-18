module Tarski.Data.Comp.Trans.DeriveUntrans (
    deriveUntrans
  ) where

import Control.Monad ( liftM )

import Data.Comp.Multi ( Alg, cata )

import Language.Haskell.TH

import Tarski.Data.Comp.Trans.Names ( baseTypes, transName, nameLab, simplifyDataInf )

--------------------------------------------------------------------------------


-- |
-- Creates an @untranslate@ function inverting the @translate@ function
-- created by @deriveTrans@.
-- 
-- @
-- import qualified Foo as F
-- type ArithTerm = Term (Arith :+: Atom :+: Lit)
-- deriveUntrans [''F.Arith, ''F.Atom, ''F.Lit] (TH.ConT ''ArithTerm)
-- @
-- 
-- will create
-- 
-- @
-- type family Targ l
-- newtype T l = T {t :: Targ l}
-- 
-- class Untrans f where
--   untrans :: Alg f t
-- 
-- untranslate :: ArithTerm l -> Targ l
-- untranslate = t . cata untrans
-- 
-- type instance Targ ArithL = F.Arith
-- instance Untrans Arith where
--   untrans (Add x y) = T $ F.Add (t x) (t y)
-- 
-- type instance Targ AtomL = F.Atom
-- instance Untrans Atom where
--   untrans (Var s)   = T $ F.Var s
--   untrans (Const x) = T $ F.Const (t x)
-- 
-- type instance Targ LitL = F.Lit
-- instance Untrans Lit where
--   untrans (Lit n) = T $ F.Lit n
-- @
-- 
-- Note that you will need to manually provide an instance @(Untrans f, Untrans g) => Untrans (f :+: g)@
-- due to phase issues.
deriveUntrans :: [Name] -> Type -> Q [Dec]
deriveUntrans names term = do targDec <- mkTarg targNm
                              wrapperDec <- mkWrapper wrapNm unwrapNm targNm
                              fnDec <- mkFn untranslateNm term targNm unwrapNm fnNm
                              classDec <- mkClass classNm fnNm wrapNm
                              instances <- liftM concat $ mapM (mkInstance classNm fnNm wrapNm unwrapNm targNm) names
                              return $ concat [ targDec
                                              , wrapperDec
                                              , fnDec
                                              , classDec
                                              , instances
                                              ]
  where
    targNm = mkName "Targ"
    wrapNm = mkName "T"
    unwrapNm = mkName "t"
    untranslateNm = mkName "untranslate"
    classNm = mkName "Untrans"
    fnNm = mkName "untrans"

{- type family Targ l -}
mkTarg :: Name -> Q [Dec]
mkTarg targNm = do i <- newName "i"
                   return [FamilyD TypeFam targNm [PlainTV i] Nothing]

{- newtype T l = T { t :: Targ l } -}
mkWrapper :: Name -> Name -> Name -> Q [Dec]
mkWrapper tpNm fNm targNm = do i <- newName "i"
                               let con = RecC tpNm [(fNm, NotStrict, AppT (ConT targNm) (VarT i))]
                               return [NewtypeD [] tpNm [PlainTV i] con []]
{-
  untranslate :: JavaTerm l -> Targ l
  untranslate = t . cata untrans
-}
mkFn :: Name -> Type -> Name -> Name -> Name -> Q [Dec]
mkFn fnNm term targNm fldNm untransNm = sequence [sig, def]
  where
    sig = do i <- newName "i"
             sigD fnNm (forallT [PlainTV i] (return []) (typ $ varT i))

    typ :: Q Type -> Q Type
    typ i = [t| $term' $i -> $targ $i |]

    term' = return term
    targ = conT targNm

    def = valD (varP fnNm) (normalB body) []

    body = [| $fld . cata $untrans |]

    fld = varE fldNm
    untrans = varE untransNm

{-
  class Untrans f where
    untrans :: Alg f T
-}
mkClass :: Name -> Name -> Name -> Q [Dec]
mkClass classNm funNm newtpNm = do f <- newName "f"
                                   let funDec = SigD funNm (AppT (AppT (ConT ''Alg) (VarT f)) (ConT newtpNm))
                                   return [ClassD [] classNm [PlainTV f] [] [funDec]]
                      
{-
  type instance Targ CompilationUnitL = J.CompilationUnit
  instance Untrans CompilationUnit where
    untrans (CompilationUnit x y z) = T $ J.CompilationUnit (t x) (t y) (t z)
-}
mkInstance :: Name -> Name -> Name -> Name -> Name -> Name -> Q [Dec]
mkInstance classNm funNm wrap unwrap targNm typNm = do inf <- reify typNm
                                                       let nmTyps = simplifyDataInf inf
                                                       clauses <- mapM (uncurry $ mkClause wrap unwrap) nmTyps
                                                       return [ famInst
                                                              , inst clauses
                                                              ]
  where
    famInst = TySynInstD targNm [ConT $ nameLab typNm] (ConT typNm)

    inst clauses =  InstanceD []
                              (AppT (ConT classNm) (ConT (transName typNm)))
                              [FunD funNm clauses]

  

mkClause :: Name -> Name -> Name -> [Type] -> Q Clause
mkClause wrap unwrap con tps = do nms <- mapM (const $ newName "x") tps
                                  return $ Clause [pat nms] (body nms) []
  where
    pat nms = ConP (transName con) (map VarP nms)

    body nms = NormalB $ AppE (ConE wrap)
                         $ foldl AppE (ConE con) (map atom $ zip nms tps)

    atom :: (Name, Type) -> Exp
    atom (x, t) | elem t baseTypes = VarE x
    atom (x, _)                    = AppE (VarE unwrap) (VarE x)