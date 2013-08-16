{-
 - Claq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Claq.
 - Claq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.ExitF (ExitF(..), exitF, contractExitF, mapExitF) where

import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

data ExitF e f a = ExitF e | ContF (f a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

exitF :: (e -> b) -> (f a -> b) -> ExitF e f a -> b
exitF g _ (ExitF e) = g e
exitF _ h (ContF f) = h f

contractExitF :: (f a -> e) -> ExitF e f a -> e
contractExitF _ (ExitF e) = e
contractExitF g (ContF f) = g f

mapExitF :: (e -> e') -> ExitF e f a -> ExitF e' f a
mapExitF g (ExitF e) = ExitF (g e)
mapExitF _ (ContF f) = ContF f
