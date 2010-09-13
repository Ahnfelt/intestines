{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Feature.Health where

import Control.Concurrent.STM
import Data.Typeable
import Data.Record.Label
import Feature

data Health = Health {
    _health :: TVar Double
    } deriving (Typeable)

$(mkLabels [''Health])

instance Supports Health l

new :: Double -> STM Health
new health = 
    return Health .$. health

