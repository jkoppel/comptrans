module Tarski.Data.Comp.Trans (
    deriveMultiComp
  , generateNameLists
  , makeSumType

  , T.deriveTrans
  , U.deriveUntrans
  ) where

import Control.Monad ( liftM )

import Data.Comp.Multi ( (:+:) )

import Language.Haskell.TH.Quote ( dataToExpQ )
import Language.Haskell.TH

import qualified Tarski.Data.Comp.Trans.DeriveTrans as T
import qualified Tarski.Data.Comp.Trans.DeriveUntrans as U
import Tarski.Data.Comp.Trans.DeriveMulti ( deriveMulti )
import Tarski.Data.Comp.Trans.Collect ( collectTypes )
import Tarski.Data.Comp.Trans.Names ( transName )

deriveMultiComp :: Name -> Q [Dec]
deriveMultiComp root = do descs <- collectTypes root
                          liftM concat $ mapM deriveMulti descs

generateNameLists :: Name -> Q [Dec]
generateNameLists root = do descs <- collectTypes root
                            nameList1 <- mkNameList (mkName "origASTTypes") descs
                            nameList2 <- mkNameList (mkName "newASTTypes") (map transName descs) 

                            return $ nameList1 ++ nameList2

mkNameList :: Name -> [Name] -> Q [Dec]
mkNameList name contents = sequence [ sigD name (appT listT (conT ''Name))
                                    , valD (varP name) (normalB namesExp) []
                                    ]
  where
    namesExp = dataToExpQ (const Nothing) contents


makeSumType :: String -> [Name] -> Q [Dec]
makeSumType nm types = sequence $ [tySynD (mkName nm) [] $ sumType types]
  where
    sumType []     = fail "Attempting to make empty sum type"
    sumType [t]    = conT t
    sumType (t:ts) = appT (appT (conT ''(:+:)) (conT t)) (sumType ts)