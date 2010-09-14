{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Feature.Cooldown where

import Data.Typeable
import Data.Record.Label
import Feature

data Type = Type {
    _total :: Var Int,
    _current :: Var Int
    } deriving (Typeable)

$(mkLabels [''Type])

instance Supports Type l

instance Updateable Type

new :: Int -> Int -> Game Type
new total current = 
    return Type .$. total .$. current

