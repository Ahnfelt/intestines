{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module Feature.Trigger where

import Control.Concurrent.STM
import Data.Typeable
import Data.Record.Label
import Feature
import Feature.Magazine
import Feature.Cooldown

data Trigger = Trigger {
    _trigger :: STM ()
    } deriving (Typeable)
    
$(mkLabels [''Trigger])

instance (Has Magazine l, Has Cooldown l) => Supports Trigger l

new :: STM () -> STM Trigger
new trigger = 
    return Trigger .$. trigger

