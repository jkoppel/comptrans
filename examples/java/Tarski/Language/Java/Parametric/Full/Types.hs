{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- This is a separate file due to GHC's phase restriction

module Tarski.Language.Java.Parametric.Full.Types where

import Data.Comp.Multi ( (:+:), (:&:), (:<:), inject, project, Cxt, Term, Alg, cata, HFunctor(..), caseH )

import qualified Language.Java.Syntax as J ( CompilationUnit )

import Data.Comp.Trans ( deriveMultiComp, makeSumType )

import Tarski.Language.Java.Parametric.Full.Names
import Tarski.Language.Parametric.Derive
import Tarski.Language.Parametric.InjF
import Tarski.SCM.Src hiding ( project )

------------------------------------------------------------

data Label = Label !Int
  deriving (Eq, Ord, Read, Show)

-----------------------------------------------------------

deriveMultiComp ''J.CompilationUnit
deriveAll newASTTypes
makeSumType "JavaSig" javaSigNames
declareAnnotatedNames "JavaSigLab" ''Label javaSigNames

data CompilationUnitIsTop e l where
  CompilationUnitIsTop :: e CompilationUnitL -> CompilationUnitIsTop e TopL

deriveAll [''CompilationUnitIsTop]

instance (CompilationUnitIsTop :<: f) => InjF (Cxt h f a) CompilationUnitL TopL where
  injF = iCompilationUnitIsTop

instance (CompilationUnitIsTop :<: f) => ProjF (Term f) CompilationUnitL TopL where
  projF (project->Just (CompilationUnitIsTop x)) = Just x
  projF _                                     = Nothing


type JavaProjSig = SourceFile :+: CompilationUnitIsTop :+: JavaSig
type JavaProjSigLab = (SourceFile :&: Label) :+: (CompilationUnitIsTop :&: Label) :+: JavaSigLab

type JavaTerm = Term JavaSig
type JavaTermLab = Term JavaSigLab
type JavaProj = Term JavaProjSig
type JavaProjLab = Term JavaProjSigLab

newtype WrapTerm f l = WrapTerm { unwrapTerm :: Term f l }

class LiftJavaProj f g where
  liftJavaProj' :: Alg f (WrapTerm g)

instance (LiftJavaProj f g, LiftJavaProj f' g) => LiftJavaProj (f :+: f') g where
  liftJavaProj' = caseH liftJavaProj' liftJavaProj'

instance (HFunctor f, f :<: g) => LiftJavaProj f g where
  liftJavaProj' = WrapTerm . inject . hfmap unwrapTerm

instance LiftJavaProj CompilationUnitIsTop JavaSig where
  liftJavaProj' = error "Found CompilationUnitIsTop when lowering Java project to term"

instance LiftJavaProj SourceFile JavaSig where
  liftJavaProj' = error "Found SourceFile when lowering Java project to term"

liftJavaProj :: JavaTerm l -> JavaProj l
liftJavaProj = unwrapTerm . cata liftJavaProj'

liftJavaProjLab :: JavaTermLab l -> JavaProjLab l
liftJavaProjLab = unwrapTerm . cata liftJavaProj'

lowerJavaProj :: JavaProj l -> JavaTerm l
lowerJavaProj = unwrapTerm . cata liftJavaProj'

