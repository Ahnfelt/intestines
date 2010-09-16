{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Feature.Health where

import Data.Typeable
import Data.Record.Label
import Feature

data Type = Type {
    _health :: Var Double
    } deriving (Typeable)

$(mkLabels [''Type])

instance Updateable Type

new :: Double -> Game Type
new health = 
    return Type .$. health

