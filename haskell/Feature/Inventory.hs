{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Feature.Inventory where

import Control.Concurrent.STM
import Data.Typeable
import Data.Record.Label
import Feature

data Inventory = Inventory {
    _inventory :: TVar [Entity]
    } deriving (Typeable)

$(mkLabels [''Inventory])

instance Supports Inventory l

new :: [Entity] -> STM Inventory
new entity = 
    return Inventory .$. entity

