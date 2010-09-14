{-# LANGUAGE FlexibleContexts, Rank2Types, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Test.Entity where

import Control.Concurrent.STM
import Control.Monad
import Data.Typeable
import Data.Maybe
import Feature

data Magazine = Magazine {
    magazines :: TVar Int,
    capacity :: TVar Int,
    ammo :: TVar Int
    } deriving (Typeable)

data Cooldown = Cooldown {
    total :: TVar Int,
    current :: TVar Int
    } deriving (Typeable)

data Trigger = Trigger {
    trigger :: STM String
    } deriving (Typeable)

instance (Has Magazine l, Has Cooldown l) => Supports Trigger l
instance Supports Magazine l
instance Supports Cooldown l

new :: (TVar Entity -> STM Entity) -> STM Entity
new constructor = do
    this <- newTVar undefined
    result <- constructor this
    writeTVar this result
    return result

method :: (Entity -> STM a) -> TVar Entity -> STM a
method function this = do
    this' <- readTVar this
    function this'

shotgun :: STM Entity
shotgun = new $ \this -> do
    magazine <- liftM3 Magazine (newTVar 10) (newTVar 2) (newTVar 1)
    cooldown <- liftM2 Cooldown (newTVar 10) (newTVar 0)
    let trigger = Trigger (method trig this)
    return $ toEntity $ trigger .:. magazine .:. cooldown .:. nil
    where
        trig this = do
            let ammo' = ammo $ fromJust $ getFeature this
            ammo'' <- readTVar ammo'
            writeTVar ammo' (ammo'' - 1)
            return ("Bang! " ++ show ammo'')

