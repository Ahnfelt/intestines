{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Feature.Magazine where

import Control.Concurrent.STM
import Data.Typeable
import Data.Record.Label
import Feature

data Magazine = Magazine {
    _magazines :: TVar Int,
    _capacity :: Int,
    _ammo :: TVar Int
    } deriving (Typeable)

$(mkLabels [''Magazine])

instance Supports Magazine l

new :: Int -> Int -> Int -> STM Magazine
new magazines capacity ammo = 
    return Magazine .$. magazines .$. capacity .$. ammo

