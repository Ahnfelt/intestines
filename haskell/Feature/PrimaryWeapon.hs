{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Feature.PrimaryWeapon where

import Control.Concurrent.STM
import Data.Typeable
import Data.Record.Label
import Feature
import Feature.Trigger

data PrimaryWeapon = PrimaryWeapon {
    _weapon :: TVar Trigger 
    } deriving (Typeable)

$(mkLabels [''PrimaryWeapon])

instance Supports PrimaryWeapon l

new :: Trigger -> STM PrimaryWeapon
new trigger = 
    return PrimaryWeapon .$. trigger

