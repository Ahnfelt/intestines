{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Feature.Trigger where

import Data.Typeable
import Data.Record.Label
import Feature
import qualified Feature.Magazine as Magazine
import qualified Feature.Cooldown as Cooldown

data Type = Type {
    _trigger :: Game ()
    } deriving (Typeable)
    
$(mkLabels [''Type])

instance Updateable Type

new :: Game () -> Game Type
new trigger = 
    return Type .$. trigger

