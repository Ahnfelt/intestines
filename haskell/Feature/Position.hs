{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Feature.Position where

import Control.Concurrent.STM
import Data.Typeable
import Data.Record.Label
import Feature
import World.Mechanics

data Type = Type {
    _position :: TVar Position
    } deriving (Typeable)

$(mkLabels [''Type])

instance Supports Type l

instance Updateable Type

new :: Position -> STM Type
new position = 
    return Type .$. position

moveBy :: Position -> Type -> STM ()
moveBy delta self = update position (.+ delta) self 

moveTo :: Position -> Type -> STM ()
moveTo target self = set position target self 

