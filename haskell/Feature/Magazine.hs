{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Feature.Magazine where

import Control.Concurrent.STM
import Data.Typeable
import Data.Record.Label
import Feature

data Type = Type {
    _magazines :: TVar Int,
    _capacity :: Int,
    _ammo :: TVar Int
    } deriving (Typeable)

$(mkLabels [''Type])

instance Supports Type l

instance Updateable Type

new :: Int -> Int -> Int -> STM Type
new magazines capacity ammo = 
    return Type .$. magazines .$. capacity .$. ammo

