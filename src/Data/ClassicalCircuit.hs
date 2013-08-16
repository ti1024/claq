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

module Data.ClassicalCircuit (ClaGate(..), ClaCircuit(..)) where

import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Data.DAG (DAG)
import qualified Data.DAG as DAG
import Data.ExitF

data ClaGate a = GConst Bool | GNot a | GAnd a a | GOr a a | GXor a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype ClaCircuit inp = ClaCircuit (DAG (ExitF inp ClaGate))

instance Functor ClaCircuit where
  fmap f (ClaCircuit g) = ClaCircuit $ DAG.mapDAG (mapExitF f) g
