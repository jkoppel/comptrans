module Tarski.Data.Comp.Trans.DeriveTrans (
    deriveTrans
  ) where

import Control.Monad ( liftM )

import Language.Haskell.TH.Syntax

deriveTrans :: Name -> [Name] -> Q [Dec]
deriveTrans root names = do classNm <- newName "Trans"
                            funNm <- newName "trans"
                            liftM (:[]) $ mkClass classNm funNm

mkClass :: Name -> Name -> Q Dec
mkClass classNm funNm = do a <- newName "a"
                           i <- newName "i"
                           let foo = mkName "Foo"
                           let transDec = SigD funNm (foldl AppT ArrowT [VarT a, AppT (ConT foo) (VarT i)])
                           return $ ClassD [] classNm [PlainTV a, PlainTV i] [] [transDec]

