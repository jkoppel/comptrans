module Tarski.Data.Comp.Trans.Collect (
    collectTypes,
    mapSetM
  ) where

import Control.Monad ( liftM, liftM2 )

import Data.Foldable ( fold )
import Data.Monoid ( Monoid(..) )

import Data.Set as Set ( Set, singleton, union, difference, toList, member, empty )

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.ExpandSyns ( expandSyns )

import Tarski.Data.Comp.Trans.Names ( standardNameSet )

collectTypes :: Name -> Q (Set Name)
collectTypes n = liftM2 difference (fixpoint collectTypes' n)
                                   (return standardNameSet)

fixpoint :: (Ord a, Monad m) => (a -> m (Set a)) -> a -> m (Set a)
fixpoint f x = run $ singleton x
  where
    run s = do s' <- liftM fold $ mapSetM f s
               if s' == s then
                 return s'
                else
                 run s'

mapSetM :: (Monad m, Ord b) => (a -> m b) -> Set a -> m (Set b)
mapSetM f x = liftM (mconcat . map singleton) $ mapM f (toList x)

collectTypes' :: Name -> Q (Set Name)
collectTypes' n | member n standardNameSet = return empty
collectTypes' n = do inf <- reify n
                     let cons = case inf of
                                      TyConI (DataD _ _ _ cns _)    -> cns
                                      TyConI (NewtypeD _ _ _ con _) -> [con]
                                      _ -> []
                     childNames <- liftM concat $ mapM extractNames cons
                     return $ (singleton n) `union` (mconcat $ map singleton childNames)
                    

class ExtractNames a where
  extractNames :: a -> Q [Name]

instance ExtractNames Con where
  extractNames (NormalC _ xs) = liftM concat $ mapM extractNames xs
  extractNames (RecC _ xs) = liftM concat $ mapM extractNames xs
  extractNames (InfixC a _ b) = liftM2 (++) (extractNames a) (extractNames b)
  extractNames (ForallC _ _ x) = extractNames x

instance ExtractNames StrictType where
  extractNames (_, t) = extractNames t

instance ExtractNames VarStrictType where
  extractNames (_, _, t) = extractNames t

instance ExtractNames Type where
  extractNames tSyn = do t <- expandSyns tSyn
                         case t of 
                           AppT a b -> liftM2 (++) (extractNames a) (extractNames b)
                           ConT n   -> return [n]  
                           _        -> return []