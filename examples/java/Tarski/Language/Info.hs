{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
-- | 
-- Module       : Tarski.Language.Info
-- Copyright    : (C) 2012-2013 Tarski Technologies, All Rights Reserved
-- Author       : James Koppel
-- 
-- This modules contains facilites for providing meta-information for
-- program terms, especially node labeling
module Tarski.Language.Info
  (
    -- GP-related metainfo
    ProgramInfo(..)
  , HasProgramInfo(..)

  , Individual(..)
  , HasIndividual(..)
     
  , ProgramResult(..)
  , HasProgramResult(..)

    -- * Labeling programs
  , Origin(..)
  , thPos
  , Label -- opaque!
  , HasLabel(..)
  , ppLabel
  , LabelGen -- opaque!
  , HasLabelGen(..)
  , mkCSLabelGen
  , mkTrivialLabelGen
  , nextLabel
  , splitLabelGen
  , annotateLabel
  , refreshLabel
  , annotateNonlabel

  , getAnn
  ) where

import Control.Concurrent.Supply ( Supply, newSupply, freshId, splitSupply )
import Control.DeepSeq.Generics ( NFData(..), genericRnf )
import Control.Lens ( Lens', (&), (.~), (^.) )
import Control.Lens.TH ( makeClassy, makeLenses )
import Control.Monad ( liftM2 )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.State ( MonadState, state, evalState  )

import Data.Binary ( Binary )
import Data.Comp.Multi ( HFunctor, Term, unTerm, (:=>), CxtFun, CxtFunM, appSigFunM, DistAnn, RemA, injectA, projectA, stripA )
import qualified Data.Comp.Ops as O ( (:&:)(..) )
import Data.Comp.Multi.HTraversable ( HTraversable )
import Data.Data ( Data )
import Data.Set ( Set )
import Data.Typeable ( Typeable )

import GHC.Generics ( Generic )

import Language.Haskell.TH.Syntax ( Q, location, Loc(..), CharPos )

import Tarski.Lens

--------------------------------------------------------------------------------
-- Meta-info about a program for GP
--------------------------------------------------------------------------------

data ProgramInfo s = ProgramInfo 
    { _coveredStatements  :: Set s  -- ^ The statements executed by any test
    , _negativeStatements :: Set s  -- ^ All statements executed by failing tests
    , _faultStatements    :: Set s  -- ^ The suspicious statements
    }

makeClassy ''ProgramInfo


data Individual l = Individual
    { _ind_contents :: l
    , _ind_parents  :: [Int]
    , _ind_id       :: Int
    } deriving (Eq, Ord, Show, Read)

makeClassy ''Individual

data ProgramResult l = ProgramResult 
    { _res_prog     :: (Individual l)  -- ^ The individual examined
    , _res_fit      :: Int             -- ^ Fitness of the individual
    , _res_compiled :: Bool            -- ^ Whether the individual compiled
    }

makeClassy ''ProgramResult

--------------------------------------------------------------------------------
-- Labeling
--------------------------------------------------------------------------------

-- Loc has no Ord instance, and this does not merit an orphaned instance
data Origin = OriginTH !String !String !String !CharPos !CharPos
            | OriginSynthetic
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance NFData Origin where rnf = genericRnf
instance Binary Origin

thPos :: Q Origin
thPos = do l <- location
           return $ OriginTH (loc_filename l)
                             (loc_package l)
                             (loc_module l)
                             (loc_start l)
                             (loc_end l)
                          
-- | Provides unique labels for AST nodes
-- 
-- In cases where we need to generate new nodes,
-- but do not care about unique labels, we can use the
-- "NonLabel" constructor. This saves us from having to use
-- a label generator.
-- 
-- In particular, we will not generate new labels
-- when applying the "insert" mutation operator
-- 
-- GenProg just does this by assigning label "0"
-- 
-- ISSUE: Perhaps the Eq and Ord instances should
-- error for NonLabel
data Label = Label !Int !Origin
           | NonLabel
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance NFData Label where rnf = genericRnf
instance Binary Label

makeClassy ''Label

ppLabel :: Label -> String
ppLabel (Label n _) = show n
ppLabel NonLabel    = "_"

data LabelGen = forall a. LabelGenInterface a => LabelGen a

class LabelGenInterface g where
  genLabel :: g -> (Label, LabelGen)
  split    :: g -> (LabelGen, LabelGen)

class HasLabelGen s where
  labelGen :: Lens' s LabelGen

instance HasLabelGen LabelGen where
  labelGen = id

nextLabel :: (MonadState s m, HasLabelGen s) => m Label
nextLabel = zoom labelGen $ state (\(LabelGen g) -> genLabel g)

-- Tried to make as instance of MonadSplit, but had overlapping instance complaints.
-- Besides, I have yet to find or discover a need for this operation to be a typeclass
splitLabelGen :: (MonadState s m, HasLabelGen s) => m LabelGen
splitLabelGen = zoom labelGen $ state (\(LabelGen g) -> split g)

--------------------------------------------------------------------------------

data ConcurrentSupplyLabelGen = ConcurrentSupplyLabelGen
    { _supply :: Supply
    , _origin :: Origin
    }
  deriving ( Eq, Ord, Show )

makeLenses ''ConcurrentSupplyLabelGen

mkCSLabelGen :: MonadIO m => Origin -> m LabelGen
mkCSLabelGen o = do s <- liftIO newSupply
                    return $ LabelGen $ ConcurrentSupplyLabelGen { _supply = s, _origin = o }

instance LabelGenInterface ConcurrentSupplyLabelGen where
  genLabel g = ( Label l (g ^. origin)
               , LabelGen (g & supply .~ s)
               )
    where
      (l, s) = freshId (g ^. supply)

  split g = ( LabelGen (g & supply .~ s)
            , LabelGen (g & supply .~ s')
            )
    where
      (s,s') = splitSupply (g ^. supply)

--------------------------------------------------------------------------------

data TrivialLabelGen = TrivialLabelGen

mkTrivialLabelGen :: LabelGen
mkTrivialLabelGen = LabelGen TrivialLabelGen

instance LabelGenInterface TrivialLabelGen where
  genLabel TrivialLabelGen = (NonLabel, LabelGen TrivialLabelGen)
  split    TrivialLabelGen = (LabelGen TrivialLabelGen, LabelGen TrivialLabelGen)

--------------------------------------------------------------------------------

-- | Fully annotates a term with fresh labels
annotateLabel :: (HTraversable f, MonadState s m, HasLabelGen s, DistAnn f Label f') => CxtFunM m f f'
annotateLabel = appSigFunM $ liftM2 injectA nextLabel . return

refreshLabel :: (HTraversable f, HFunctor f, HFunctor f', MonadState s m, HasLabelGen s, RemA f' f, DistAnn f Label f') => CxtFunM m f' f'
refreshLabel = annotateLabel . stripA

annotateNonlabel :: (HTraversable f, DistAnn f Label f') => CxtFun f f'
annotateNonlabel t = evalState (annotateLabel t) mkTrivialLabelGen

-- | Extract the annotation from the top of an annotated term
getAnn :: (DistAnn s p s') => Term s' :=> p
getAnn = annSnd . projectA . unTerm
  where
    annSnd (_ O.:&: x) = x