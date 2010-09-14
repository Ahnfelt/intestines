{-# LANGUAGE FlexibleContexts, Rank2Types, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TemplateHaskell, TypeOperators, FunctionalDependencies #-}

module Test.Entity2 where

import Control.Concurrent.STM
import Control.Monad
import Data.Typeable
import Data.Maybe
import Data.Record.Label
import Feature

data Magazine = Magazine {
    _magazines :: TVar Int,
    _capacity :: Int,
    _ammo :: TVar Int
    } deriving (Typeable)

data Cooldown = Cooldown {
    _total :: TVar Int,
    _current :: TVar Int
    } deriving (Typeable)

data Trigger = Trigger {
    _trigger :: STM String
    } deriving (Typeable)

$(mkLabels [''Magazine, ''Cooldown, ''Trigger])


instance (Has Magazine l, Has Cooldown l) => Supports Trigger l
instance Supports Magazine l
instance Supports Cooldown l

newMagazine :: Int -> Int -> Int -> STM Magazine
newMagazine magazines capacity ammo = 
    return Magazine .$. magazines .$. capacity .$. ammo

shotgun :: STM Entity
shotgun = new $ \this -> do
    magazine <- newMagazine 10 2 1
    cooldown <- return Cooldown .$. int 10 .$. int 0
    trigger <- return Trigger .$. method trig this
    return $ toEntity $ trigger .:. magazine .:. cooldown .:. nil
    where
        trig this = do
            let magazine = fromJust $ getFeature this
            update ammo (flip (-) 1) magazine
            ammo' <- get ammo magazine
            return ("Bang! " ++ show ammo')

test = do
    gun <- atomically shotgun
    replicateM_ 10 $ do
        res <- atomically $ getL trigger $ fromJust $ getFeature gun
        print res


