{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Feature.Inventory where

import Data.Typeable
import Data.Record.Label
import Feature

data Type = Type {
    _inventory :: Var [Entity ()]
    } deriving (Typeable)

$(mkLabels [''Type])

instance Updateable Type

new :: [Entity ()] -> Game Type
new entity = 
    return Type .$. entity

