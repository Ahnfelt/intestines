{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Feature.PrimaryWeapon where

import Control.Concurrent.STM
import Data.Typeable
import Data.Record.Label
import Feature
import qualified Feature.Trigger as Trigger

data Type = Type {
    _weapon :: TVar Trigger.Type 
    } deriving (Typeable)

$(mkLabels [''Type])

instance Supports Type l

instance Updateable Type

new :: Trigger.Type -> STM Type
new trigger = 
    return Type .$. trigger

