{-
 - Claq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Claq.
 - Claq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compile (prepareExprCoherent, prepareExprNoncoherent) where

import Data.Functor ((<$>))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Traversable (traverse)

import Control.Monad.Quantum.Class
import Data.Quantum.Wire

import Data.ClassicalCircuit
import qualified Data.DAG as DAG
import Data.ExitF

prepareExprGraph :: MonadQuantum Wire m => ClaCircuit (Bit Wire) -> m (Seq (Bit Wire))
prepareExprGraph (ClaCircuit g) =
    DAG.foldMapDAGM (exitF return prepareNode) g
  where
    prepareNode (GConst c) = return $ BitConst c
    prepareNode (GNot b1) = return $ negateBit b1
    prepareNode (GAnd (BitConst False) _) = return $ BitConst False
    prepareNode (GAnd (BitConst True) b2) = return b2
    prepareNode (GAnd _ (BitConst False)) = return $ BitConst False
    prepareNode (GAnd b1 (BitConst True)) = return b1
    prepareNode (GAnd b1@(BitWire inv1 w1) b2@(BitWire inv2 w2))
      | w1 == w2 && inv1 == inv2 =
          return b1
      | w1 == w2 && inv1 /= inv2 =
          return $ BitConst False
      | otherwise = do
          w <- ancilla
          destructiveToffoli w b1 b2
          return $ bit w
    prepareNode (GOr b1 b2) = negateBit <$> prepareNode (GAnd (negateBit b1) (negateBit b2))
    prepareNode (GXor (BitConst False) b2) = return b2
    prepareNode (GXor (BitConst True) b2) = return $ negateBit b2
    prepareNode (GXor b1 (BitConst False)) = return b1
    prepareNode (GXor b1 (BitConst True)) = return $ negateBit b1
    prepareNode (GXor b1@(BitWire inv1 w1) b2@(BitWire inv2 w2))
      | w1 == w2 =
          return $ BitConst $ inv1 /= inv2
      | otherwise = do
          w <- ancilla
          control b1 $ applyX w
          control b2 $ applyX w
          return $ bit w

prepareExprCoherent :: MonadQuantum Wire m => ClaCircuit (Bit Wire) -> Seq Int -> m (Seq Wire)
prepareExprCoherent g vs =
    with (prepareExprGraph g) $ \ws ->
      traverse (prepareCopy ws) vs

prepareExprNoncoherent :: MonadQuantum Wire m => ClaCircuit (Bit Wire) -> Seq Int -> m (Seq Wire)
prepareExprNoncoherent g vs = do
    ws <- prepareExprGraph g
    traverse (prepareCopy ws) vs

prepareCopy :: MonadQuantum w m => Seq (Bit w) -> Int -> m w
prepareCopy ws v = do
    w <- ancilla
    cnotWire w (Seq.index ws v)
    return w
