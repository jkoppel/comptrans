{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | 
-- Module       : Tarski.Language.Parametric.InjF
-- Copyright    : (C) 2012-2013 Tarski Technologies, All Rights Reserved
-- Author       : James Koppel
-- 
-- This module enables the creation of "sort injections," stating that
-- one sort can be considered a coercive subsort of another
module Tarski.Language.Parametric.InjF
  (
    InjF
  , ProjF
  , injF
  , projF
  , injectF
  ) where

import Control.Monad ( liftM )

import Data.Comp.Multi ( Cxt(..), (:<:), (:+:), Cxt, inj, inject, caseH )
import Data.Comp.Multi.HFunctor ( K(..) )

--------------------------------------------------------------------------------

-- |
-- InjF allows us to create "sort injections," stating that one sort can be considered
-- a coercive subsort of another..
-- 
-- For example, if we wanted to parameterize whether a given syntax
-- allows arbitrary expressions to be used as function arguments,
-- we could have the function terms have arguments of sort "FunArg"
-- and create an "ExpressionIsFunArg" . Defining an instance
-- 
-- > instance (ExpressionIsFunArg :<: f) => InjF (Term f) ExpL FunArgL
-- 
-- would then allow us to use expression as function arguments freely.
-- 
-- Currently, the only example is 'Tarski.Language.Java.Parametric.Full.CompilationUnitIsTop'
class InjF f l l' where
  injF :: f l -> f l'

-- |
-- Dynamically casing on subsorts
class ProjF f l l' where
  projF :: f l' -> Maybe (f l)

instance InjF f l l where
  injF = id

instance ProjF f l l where
  projF = Just

instance (f :<: f :+: g, g :<: f :+: g, ProjF (f e) l l', ProjF (g e) l l') => ProjF ((f :+: g) e) l l' where
  projF = caseH (liftM inj . projF) (liftM inj . projF)

instance (ProjF (K a) l l') where
  projF = Just . K . unK

instance (ProjF (f (Cxt h f a)) l l', ProjF a l l') => ProjF (Cxt h f a) l l' where
  projF (Term x) = liftM Term $ projF x
  projF (Hole x) = liftM Hole $ projF x

-- | 'injF' but for terms. Or 'inject', but allowing sort injections
-- We would like this to replace the 'inject' function outright
injectF :: (g :<: f, InjF (Cxt h f a) l l') => g (Cxt h f a) l -> Cxt h f a l'
injectF = injF . inject
