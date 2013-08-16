{-
 - Claq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Claq.
 - Claq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

module Util (mapFromListNoDup, mapFromAscListNoDup) where

import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

-- |Converts an associative list to a `Map`.
-- If the given list contains duplicate keys, it evaluates to @Left k@
-- where @k@ is one of the duplicate keys.
mapFromListNoDup :: Ord k => [(k, a)] -> Either k (Map k a)
mapFromListNoDup = mapFromAscListNoDup . sortBy (compare `on` fst)

-- |The same as `mapFromListNoDup`, but assumes that the keys are sorted
-- in the nondecreasing order.
mapFromAscListNoDup :: Eq k => [(k, a)] -> Either k (Map k a)
mapFromAscListNoDup xs =
    case dupKey of
      Nothing ->
        Right $ Map.fromAscList xs
      Just k ->
        Left k
  where
    dupKey = case xs of
      [] -> Nothing
      _ : xs' ->
        listToMaybe $ catMaybes $ zipWith (\(k, _) (k', _) -> if k == k' then Just k else Nothing) xs xs'
