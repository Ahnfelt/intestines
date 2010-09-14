{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Feature.Cooldown where

import Control.Concurrent.STM
import Data.Typeable
import Data.Record.Label
import Feature

data Type = Type {
    _total :: TVar Int,
    _current :: TVar Int
    } deriving (Typeable)

$(mkLabels [''Type])

instance Supports Type l

instance Updateable Type

new :: Int -> Int -> STM Type
new total current = 
    return Type .$. total .$. current

