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

import Control.Lens.TH ( makeClassy )


import GHC.Generics ( Generic )

import Tarski.Language.Info
import Tarski.Language.Parametric.Derive
import Tarski.Language.Parametric.InjF

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
