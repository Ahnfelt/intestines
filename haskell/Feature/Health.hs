{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Feature.Health where

import Control.Concurrent.STM
import Data.Typeable
import Data.Record.Label
import Feature

data Type = Type {
    _health :: TVar Double
    } deriving (Typeable)

$(mkLabels [''Type])

instance Supports Type l

instance Updateable Type

new :: Double -> STM Type
new health = 
    return Type .$. health

