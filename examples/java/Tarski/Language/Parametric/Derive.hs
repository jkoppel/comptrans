{-# LANGUAGE TemplateHaskell #-}
-- | 
-- Module       : Tarski.Language.Parametric.Derive
-- Copyright    : (C) 2012-2013 Tarski Technologies, All Rights Reserved
-- Author       : James Koppel
-- 
-- A utility wrapper around both Data.Comp.Multi.Derive and
-- the comptrans library, allowing us to easily derive all desired instances
-- for a higher-order functor, including for Tarski's typeclasses

module Tarski.Language.Parametric.Derive
  (
    deriveAll
  , distributeAnnotation
  , declareAnnotatedNames
  , declareAnnotated
  ) where

import Data.Comp.Multi ( (:+:), (:&:) )
import Data.Comp.Multi.Derive ( derive, makeHFunctor, makeHTraversable, makeHFoldable, makeEqHF, makeShowHF, makeOrdHF )

import Data.Functor ( (<$>) )

import Language.Haskell.TH.ExpandSyns ( expandSyns )
import Language.Haskell.TH.Syntax

import Tarski.Sin.Compdata.Derive ( smartFConstructors, makeDynCase )

--------------------------------------------------------------------------------

-- | Derives instances of the following for each type in the list:
-- 
-- @
-- 'HFunctor', 'HTraversable', 'HFoldable', 'EqHF', 'ShowHF', 'OrdHF', 'DynCase'
-- @
-- 
-- Additonally, it will create smart constructors for the data type
deriveAll :: [Name] -> Q [Dec]
deriveAll = derive [makeHFunctor, makeHTraversable, makeHFoldable, makeEqHF, makeShowHF,
                    makeOrdHF, smartFConstructors, makeDynCase]

-- | Distributes an annotation over a sum
distributeAnnotation :: Type -> Name -> Type
distributeAnnotation typ ann = dist typ
  where
    dist :: Type -> Type
    dist (AppT (AppT (ConT c) l) r)
         | c == ''(:+:)             = AppT (AppT (ConT ''(:+:)) (dist l)) (dist r) -- parser complained about pattern
    dist t                          = AppT (AppT (ConT ''(:&:)) t) (ConT ann)

declareAnnotatedType :: String -> Name -> Type -> [Dec]
declareAnnotatedType s ann typ = [TySynD (mkName s) [] annTyp]
  where
    annTyp = distributeAnnotation typ ann

-- | @declareAnnotatedNames s l ts@ will declare @s@ to be the sum of the @ts@, each annotated
-- with @l@. It corresponds roughly to the following type-level pseudocode:
-- 
-- @
-- type s = fold (:+:) (map (\t -> t :&: l) ts)
-- @
declareAnnotatedNames :: String -> Name -> [Name] -> Q [Dec]
declareAnnotatedNames _ _ []    = fail "declareAnnotatedNames: Name list empty"
declareAnnotatedNames s ann nms = return $ declareAnnotatedType s ann $ foldr typeSum (ConT $ last nms) (map ConT $ init nms)
  where
    typeSum a b = AppT (AppT (ConT ''(:+:)) a) b

declareAnnotated :: String -> Name -> Name -> Q [Dec]
declareAnnotated s ann nm = declareAnnotatedType s ann <$> expandSyns (ConT nm)