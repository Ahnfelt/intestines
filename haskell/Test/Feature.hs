{-# LANGUAGE FlexibleContexts, Rank2Types, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Test.Feature where

import Data.Typeable
import Feature

data Position = Position (Int, Int) deriving (Show, Typeable)
data Health = Health Int deriving (Show, Typeable)
data Movable = Movable deriving (Show, Typeable)

instance Has Position l => Supports Movable l
instance Supports Position l
instance Supports Health l

p1 = Movable .:. Health 10 .:. Position (3, 4) .:. nil
p2 = Health 9 .:. Position (5, 3) .:. nil
p3 = Position (5, 3) .:. nil
p4 = Health 9 .:. nil

p5 = p3 +++ p4

type Person a = (Has Position a, Has Health a) => a

showPerson :: (Has Position a, Has Health a) => Person a -> String
showPerson a = show (has a :: Position) ++ show (has a :: Health)

Just (health, position) = getFeatures (toEntity p1) :: Maybe (Health, Position)

