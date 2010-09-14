{-# LANGUAGE FlexibleContexts, Rank2Types, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeOperators, TypeFamilies, TypeSynonymInstances #-}

module Test.Has where

import Control.Concurrent.STM
import Control.Monad
import Data.Typeable
import Data.Maybe
import Data.Has
import Feature

data Magazines = Magazines; type instance TypeOf Magazines = TVar Int
data Capacity = Capacity; type instance TypeOf Capacity = TVar Int
data Ammo = Ammo; type instance TypeOf Ammo = TVar Int

newtype Magazine = Magazine (FieldOf Magazines :&: FieldOf Capacity :&: FieldOf Ammo)

data Total = Total; type instance TypeOf Total = TVar Int
data Current = Current; type instance TypeOf Current = TVar Int

newtype Cooldown = Cooldown (FieldOf Total :&: FieldOf Current)

data Trig = Trig; type instance TypeOf Trig = STM String

newtype Trigger = Trigger (FieldOf Trig)

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
    magazine <- liftM Magazine $ Magazines <-- 10 .&. Capacity <-- 2 .&. Ammo <-- 1
    cooldown <- liftM Cooldown $ Total <-- 10 .&. Current <-- 0
    let trigger = liftM Trigger $ Trig <-- method trig this
    return $ toEntity $ trigger .:. magazine .:. cooldown .:. HNil
    where
        trig this = do
            update Ammo ((-)1) this
            ammo <- get Ammo this
            return ("Bang! " ++ show ammo)

update :: (Contains (Labelled label (TVar value)) record) =>
    label -> (value -> value) -> record -> STM ()
update label f record = do
    let tvar = label ^. record
    value <- readTVar tvar
    writeTVar tvar (f value)

get :: (Contains (Labelled label (TVar value)) record) =>
    label -> record -> STM value
get label record = readTVar (label ^. record)

set :: (Contains (Labelled label (TVar value)) record) =>
    label -> value -> record -> STM ()
set label value record = writeTVar (label ^. record) value

class Initialize value where
    (<--) :: label -> value -> STM (label :> value)
    
instance Initialize (TVar value) where
    label <-- value = newTVar value >>= (label .>)

instance Initialize value where
    label <-- value = return $ label .> value

--(.&.) :: (Append a b) => STM a -> STM b -> STM (a :&: b)
a .&. b = liftM2 (&) a b

