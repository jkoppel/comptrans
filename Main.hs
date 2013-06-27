module Main where

import qualified Language.Java.Syntax as J
import Tarski.Data.Comp.Trans.Collect
import Tarski.Data.Comp.Trans.DeriveMulti

import Language.Haskell.TH
import Data.Functor
import Control.Monad
import Data.Set (toList)


$(do s <- collectTypes ''J.CompilationUnit
     liftM Prelude.concat $ mapM deriveMulti $ toList s)

foo = BooleanT