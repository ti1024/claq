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

module Language.Claq.Syntax (Stmt(..), stmtsToCircuit) where

import Control.Arrow
import Control.Monad.Free
import Control.Monad.State
import Data.Foldable (Foldable, traverse_)
import Data.Functor ((<$>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple (swap)
import Data.Traversable (Traversable, mapAccumL, traverse)

import Data.ClassicalCircuit
import qualified Data.DAG as DAG
import Data.ExitF
import Util (mapFromListNoDup)

type Expr v = Free ClaGate v

data Stmt v = SInputs [v] | SOutputs [Expr v] | SEquation v (Expr v)
  deriving (Show, Functor, Foldable, Traversable)

data StateVar v = StateVar {
  stateInputs :: Maybe [v],
  stateOutputs :: Maybe [Expr v],
  stateEquations :: [(v, Free ClaGate v)]
}

stmtsToCircuit :: (Ord v, Show v) => [Stmt v] -> Either String (ClaCircuit v, [v], [Int], Map v Int)
stmtsToCircuit stmts = do
    s <- execStateT (traverse_ processStmt stmts) (StateVar Nothing Nothing [])
    inputs <- case stateInputs s of
      Nothing -> Left "missing .inputs statement"
      Just inputs -> return inputs
    outputs <- case stateOutputs s of
      Nothing -> Left "missing .outputs statement"
      Just outputs -> return outputs
    let inassocs = map (\instr -> (instr, Free $ ExitF instr)) inputs
        eqassocs = map (second (hoistFree ContF)) $ stateEquations s
    m <- case mapFromListNoDup $ inassocs ++ eqassocs of
      Left v ->
        Left $ "wire " ++ show v ++ " defined more than once"
      Right m -> return m
    (varToInt, g) <- either (Left . showDAGError) return $ DAG.fromExprs m
    let lookupVar k =
          case Map.lookup k varToInt of
            Nothing -> Left $ notFoundError k
            Just v -> Right v
        lookupExpr e =
          hoistFree ContF <$> traverse lookupVar e
    outs <- traverse lookupExpr outputs
    let (g', ovs) = mapAccumL (\g1 o -> swap $ DAG.insertExpr o g1) g outs
    return (ClaCircuit g', inputs, ovs, varToInt)
  where
    processStmt (SInputs inputs) = do
        inputs' <- gets stateInputs
        case inputs' of
          Nothing ->
            modify $ \s -> s {stateInputs = Just inputs}
          Just _ ->
            lift $ Left "duplicate .inputs statements"
    processStmt (SOutputs outputs) = do
        outputs' <- gets stateOutputs
        case outputs' of
          Nothing ->
            modify $ \s -> s {stateOutputs = Just outputs}
          Just _ ->
            lift $ Left "duplicate .outputs statements"
    processStmt (SEquation v fr) =
        modify $ \s -> s {stateEquations = (v, fr) : stateEquations s}
    showDAGError (DAG.NotFound v) =
        notFoundError v
    showDAGError (DAG.Cyclic v) =
        "circuit contains cycle (including wire " ++ show v ++ ")"
    notFoundError k =
        "wire " ++ show k ++ " not found"
