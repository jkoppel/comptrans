{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE TemplateHaskell #-}

-- This is in a separate file due to GHC's phase restriction

module Tarski.Language.Java.Parametric.Full.Names (
    origASTTypes
  , newASTTypes
  , javaSigNames
  ) where

import qualified Language.Haskell.TH as TH ( Name )
import Language.Java.Syntax

import Data.Comp.Trans ( generateNameLists )

import Tarski.Language.Parametric.Syntax.Functor

generateNameLists ''CompilationUnit

javaSigNames :: [TH.Name]
javaSigNames = newASTTypes ++ [''PairF, ''ListF, ''MaybeF]