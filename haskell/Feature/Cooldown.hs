{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Feature.Cooldown where

import Control.Concurrent.STM
import Data.Typeable
import Data.Record.Label
import Feature

data Cooldown = Cooldown {
    _total :: TVar Int,
    _current :: TVar Int
    } deriving (Typeable)

$(mkLabels [''Cooldown])

instance Supports Cooldown l


new :: Int -> Int -> STM Cooldown
new total current = 
    return Cooldown .$. total .$. current

