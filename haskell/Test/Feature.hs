{-# LANGUAGE FlexibleContexts, Rank2Types, DeriveDataTypeable #-}

module Test.Feature where

import Data.Typeable
import Feature

data Position = Position (Int, Int) deriving (Show, Typeable)
data Health = Health Int deriving (Show, Typeable)

p1 = Position (3, 4) .:. Health 10 .:. nil
p2 = Health 9 .:. Position (5, 3) .:. nil
p3 = Position (5, 3) .:. nil
p4 = Health 9 .:. nil

p5 = p3 +++ p4

type Person a = (Has Position a, Has Health a) => a

showPerson :: (Has Position a, Has Health a) => Person a -> String
showPerson a = show (has a :: Position) ++ show (has a :: Health)

Just (health, position) = getFeatures (toEntity p1) :: Maybe (Health, Position)

