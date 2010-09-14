{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Feature.Inventory where

import Control.Concurrent.STM
import Data.Typeable
import Data.Record.Label
import Feature

data Type = Type {
    _inventory :: TVar [Entity]
    } deriving (Typeable)

$(mkLabels [''Type])

instance Supports Type l

instance Updateable Type

new :: [Entity] -> STM Type
new entity = 
    return Type .$. entity

