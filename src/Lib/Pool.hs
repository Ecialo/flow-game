{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Pool
  ( newPool,
    insert,
    delete,
    lookup,
    update,
    Pool,
  )
where

import Data.Functor
import Control.Applicative
import Data.Foldable
import Data.Traversable
import qualified Data.IntMap.Strict as Map
import Prelude (Int, Maybe, ($), (+))

data Pool a = Pool
  { storage :: Map.IntMap a,
    counter :: Map.Key
  }

instance Functor Pool where
  f `fmap` pool@Pool {storage = storage_} = pool {storage = f `fmap` storage_}
  
instance Foldable Pool where
  foldr f i pool = foldr f i (storage pool)
  
instance Traversable Pool where
  traverse f pool@Pool {storage = storage_} = pure poolFromStorage <*> tstorage
    where tstorage = traverse f storage_
          poolFromStorage str = pool {storage = str}

newPool :: Pool a
newPool = Pool {storage = Map.empty, counter = 0}

insert :: Pool a -> (Map.Key -> a) -> (Map.Key, Pool a)
insert Pool {storage = storage_, counter = counter_} value = (counter_, updatedPool)
  where
    updatedStorage = Map.insert counter_ (value counter_) storage_
    updatedPool = Pool {storage = updatedStorage, counter = counter_ + 1}

lookup :: Map.Key -> Pool a -> Maybe a
lookup key pool = Map.lookup key $ storage pool

update :: (a -> Maybe a) -> Map.Key -> Pool a -> Pool a
update f key pool = Pool {counter = counter pool, storage = Map.update f key $ storage pool}

delete :: Map.Key -> Pool a -> Pool a
delete key pool = pool {storage = Map.delete key $ storage pool}
