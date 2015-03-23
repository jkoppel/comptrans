{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
-- | 
-- Module       : Tarski.SCM.Src
-- Copyright    : (C) 2012-2013 Tarski Technologies, All Rights Reserved
-- Author       : James Koppel
-- 
--  This module provides types and functions for dealing
--  with source files, including finding dependencies and copying a project.
module Tarski.SCM.Src
  (
    SourceFile(..)
  , SourceFileL
  , iSourceFile
  , TopL
  ) where

import Control.Applicative ( Applicative )
import Control.Exception ( bracket ) 

import Control.Lens ( (&), (^.), (.~) )
import Control.Lens.TH ( makeClassy )

import Control.Monad ( liftM )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Trans.Maybe ( MaybeT )

import Data.Binary ( Binary )
import Data.Comp.Multi ( (:<:), Cxt(..), Term, DistAnn, HFunctor, RemA, ann, appCxt, project' )
import Data.Comp.Multi.HTraversable ( HTraversable )
import qualified Data.Comp.Multi as M ( project )
import Data.Data ( Data )
import Data.Typeable ( Typeable )

import GHC.Generics ( Generic )

import System.Directory ( getCurrentDirectory, setCurrentDirectory )
import System.Exit ( ExitCode )
import System.FilePath ( combine, takeExtension )
import System.Process ( system )

import Text.JSON ( JSValue(..), fromJSObject )
import Text.JSON.String ( runGetJSON, readJSObject )
import Text.PrettyPrint.GenericPretty ( Out )
import Text.Printf ( printf )

import Tarski.Language.Info
import Tarski.Log
import Tarski.Language.Parametric.Classification
import Tarski.Language.Parametric.Derive
import Tarski.Language.Parametric.InjF
import Tarski.Language.Parametric.Strategic
import Tarski.Language.Parametric.Syntax.Functor

--------------------------------------------------------------------------------

-- | A sort denoting the top-level sort seen in a source file. Use via subsorts
data TopL 

data SourceFileL
data SourceFile e l where
  SourceFile :: Bool     -- whether the file is changed. ISSUE: ought be external annotation
             -> FilePath -- File path of the corresponding source file
             -> e TopL   -- Contents of the source file
             -> SourceFile e SourceFileL

deriveAll [''SourceFile]
