{-
 - Claq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Claq.
 - Claq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.DAG (
  DAGConstructionError(..),
  DAG, adjs, empty, insert, null, size, view,
  fromTopSortSeq, find,
  mapDAG, foldMapDAG, foldMapDAGM,
  insertExpr, fromExprs) where

import Prelude hiding (null)
import Control.Arrow
import Control.Monad.Free
import Control.Monad.State
import Data.Foldable (foldlM, traverse_)
import Data.Functor ((<$>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Sequence (Seq, (|>), ViewR((:>)))
import qualified Data.Sequence as Seq
import Data.Traversable (Traversable, traverse)

data DAGConstructionError k =
    NotFound k | Cyclic k
  deriving Show

-- |Directed acyclic graph (DAG).  Nodes are labeled by @Int@s
-- from 0 to n - 1, where n is the number of nodes in the DAG.
-- The adjacency list of a node is represented by a value of type @f Int@.
-- @f@ is expected to be a `Traversable`.
-- Each node @v@ can only refer to nodes with labels less than @v@.
-- A DAG is defined inductively: it is either empty or a node v and a DAG G,
-- where node v possibly has edges to the nodes in G.
newtype DAG f = DAG {
  adjs :: Seq (f Int)
}

deriving instance Show (f Int) => Show (DAG f)

-- |Empty DAG.
empty :: DAG f
empty = DAG Seq.empty

-- |@insert adj g@ is a pair @(v, g')@,
-- where @g'@ is the DAG obtained by adding a new node @v@ to DAG @g@,
-- and the adjacency list of @v@ is @adj@.
-- All elements in @adj@ must be vertices of @g@.
insert :: f Int -> DAG f -> (Int, DAG f)
insert adj (DAG s) =
    (Seq.length s, DAG (s |> adj))

-- |@True@ if and only if the DAG is empty.
null :: DAG f -> Bool
null = Seq.null . adjs

-- |The number of nodes in a DAG.
size :: DAG f -> Int
size = Seq.length . adjs

view :: DAG f -> Maybe (Int, f Int, DAG f)
view (DAG s) =
    case Seq.viewr s of
      Seq.EmptyR -> Nothing
      s' :> adj -> Just (Seq.length s', adj, DAG s')

fromTopSortSeq :: Seq (f Int) -> DAG f
fromTopSortSeq = DAG

find :: Int -> DAG f -> f Int
find v (DAG s) =
    Seq.index s v

mapDAG :: (f Int -> f' Int) -> DAG f -> DAG f'
mapDAG f (DAG s) =
    DAG (f <$> s)

foldMapDAG :: Functor f => (f a -> a) -> DAG f -> Seq a
foldMapDAG f (DAG s) =
    s'
  where
    s' = f . fmap (Seq.index s') <$> s

foldMapDAGM :: (Functor f, Monad m) => (f a -> m a) -> DAG f -> m (Seq a)
foldMapDAGM f (DAG s) =
    foldlM g Seq.empty s
  where
    g s' adj = liftM (s' |>) $ f (Seq.index s' <$> adj)

insertExpr :: Traversable f => Free f Int -> DAG f -> (Int, DAG f)
insertExpr e =
    runState (expand e)
  where
    expand (Pure v) =
        return v
    expand (Free f) = do
        f' <- traverse expand f
        g <- get
        let (v, g') = insert f' g
        put g'
        return v

fromExprs :: (Ord k, Traversable f) => Map k (Free f k) -> Either (DAGConstructionError k) (Map k Int, DAG f)
fromExprs mf =
    first (fmap fromJust) <$> execStateT (traverse_ expandKey $ Map.keys mf) (Map.empty, empty)
  where
    expandKey k = do
        keyToIntMap <- gets fst
        case Map.lookup k keyToIntMap of
          Just (Just v) -> return v
          Just Nothing ->
            lift $ Left $ Cyclic k
          Nothing -> do
            modify $ first (Map.insert k Nothing)
            v <- case Map.lookup k mf of
              Nothing -> lift $ Left $ NotFound k
              Just fr -> expand fr
            modify $ first (Map.insert k (Just v))
            return v
    expand (Pure k) =
        expandKey k
    expand (Free f) = do
        f' <- traverse expand f
        g <- gets snd
        let (v, g') = insert f' g
        modify $ second (const g')
        return v
