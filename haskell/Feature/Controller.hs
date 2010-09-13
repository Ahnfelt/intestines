{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module Feature.Controller where

import Control.Concurrent.STM
import Data.Typeable
import Data.Record.Label
import Feature
import qualified Feature.Position as Position
import Feature.Inventory

data Controller = Controller {
    _controller :: STM ()
    } deriving (Typeable)
    
$(mkLabels [''Controller])

instance (Has Position.Type l, Has Inventory l) => Supports Controller l

new :: STM () -> STM Controller
new controller = 
    return Controller .$. controller

