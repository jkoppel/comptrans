{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- This is a separate file due to GHC's phase restriction.

module Tarski.Language.Java.Parametric.Full.Trans (
    translate
  , untranslate
  ) where

import Data.Comp.Multi ( caseH, (:+:) )

import qualified Language.Java.Syntax as J
import qualified Language.Haskell.TH as TH

import Data.Comp.Trans ( deriveTrans, deriveUntrans )

import Tarski.Language.Java.Parametric.Full.Names
import Tarski.Language.Java.Parametric.Full.Types
import Tarski.Language.Parametric.Syntax.Functor

deriveTrans ''J.CompilationUnit origASTTypes (TH.ConT ''JavaTerm)

instance (Trans c l) => Trans [c] [l] where
  trans [] = riNilF
  trans (x:xs) = (trans x :: JavaTerm l) `iConsF` (trans xs)

instance (Trans c l) => Trans (Maybe c) (Maybe l) where
  trans Nothing = riNothingF
  trans (Just x) = iJustF $ (trans x :: JavaTerm l)

instance (Trans c l, Trans d l') => Trans (c, d) (l, l')  where
  trans (x, y) = riPairF (trans x) (trans y)



deriveUntrans origASTTypes (TH.ConT ''JavaTerm)

type instance Targ [l] = [Targ l]
instance Untrans ListF where
  untrans NilF = T []
  untrans (ConsF a b) = T ((t a) : (t b))

type instance Targ (Maybe l) = Maybe (Targ l)
instance Untrans MaybeF where
  untrans NothingF = T Nothing
  untrans (JustF x) = T (Just (t x))

type instance Targ (l, l') = (Targ l, Targ l')
instance Untrans PairF where
  untrans (PairF x y) = T (t x, t y)

instance (Untrans f, Untrans g) => Untrans (f :+: g) where
  untrans = caseH untrans untrans