{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

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

instance (Has Magazine.Type l, Has Cooldown.Type l) => Supports Type l

instance Updateable Type

new :: Game () -> Game Type
new trigger = 
    return Type .$. trigger

