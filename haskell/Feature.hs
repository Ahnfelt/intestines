{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, TemplateHaskell, TypeOperators #-}
-- NOTE: UndecidableInstances is required by the Combine type class,
--       which is a restricted copy of HAppend in the HList module.
--       It can be removed yielding a slightly less precise system.
--       It is also used for the Has type class synonym.

module Feature (
    Supports (..), (.:.), Combine ((+++)), 
    Entity, toEntity, getFeature, GetFeatures (..), 
    int, Convert (..), (.$.), 
    new, method, 
    get, set, update,
    Has, has, nil) where

import Control.Concurrent.STM
import Data.HList
import Data.Dynamic
import Data.Maybe
import Data.Record.Label


int :: Int -> Int
int = id

class Convert a b where
    convert :: a -> STM b

instance Convert a a where
    convert = return

instance Convert a (TVar a) where
    convert a = newTVar a

(.$.) :: (Convert a a') => STM (a' -> b) -> a -> STM b
f .$. v = do 
    f' <- f
    v' <- convert v
    return $ f' v'


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
    

get :: (record :-> TVar value) -> record -> STM value
get label record = readTVar (getL label record)

set :: (record :-> TVar value) -> value -> record -> STM ()
set label value record = writeTVar (getL label record) value

update :: (record :-> TVar value) -> (value -> value) -> record -> STM ()
update label f record = do
    let tvar = getL label record
    value <- readTVar tvar
    writeTVar tvar (f value)


newtype Entity = Entity [Dynamic]


class Supports f l

infixr 6 .:.
(.:.) :: (Supports e l, HOccursNot e l, HExtend e l l') => e -> l -> l'
a .:. b = a .*. b


class Combine l l' l'' | l l' -> l'' where
    (+++) :: l -> l' -> l''

instance HList l => Combine HNil l l where
    HNil +++ l = l

instance (HList l, Combine l l' l'', HOccursNot x l') => Combine (HCons x l) l' (HCons x l'') where
    HCons x l +++ l' = HCons x (l +++ l')


class HOccurs e l => Has e l
instance HOccurs e l => Has e l
has l = hOccurs l
nil = HNil

data ToDynamic = ToDynamic

instance (Typeable a) => Apply ToDynamic a Dynamic where 
    apply ToDynamic = toDyn

toEntity :: (HMapOut ToDynamic r Dynamic) => r -> Entity
toEntity l = Entity $ hMapOut ToDynamic l


getFeature :: Typeable e => Entity -> Maybe e
getFeature (Entity ds) = case catMaybes $ map fromDynamic ds of
    e:_ -> Just e
    [] -> Nothing

class GetFeatures l where
    getFeatures :: Entity -> Maybe l

instance GetFeatures HNil where
    getFeatures _ = Just HNil

instance (GetFeatures l, Typeable e) => GetFeatures (HCons e l) where
    getFeatures (Entity ds) = case catMaybes $ map fromDynamic ds of
        e:_ -> case getFeatures (Entity ds) of
            Just l -> Just (HCons e l)
            Nothing -> Nothing
        [] -> Nothing

instance (Typeable e1, Typeable e2) => GetFeatures (e1, e2) where
    getFeatures (Entity ds) = case getFeatures (Entity ds) of
        Just (HCons e1 (HCons e2 HNil)) -> Just (e1, e2)
        Nothing -> Nothing

instance (Typeable e1, Typeable e2, Typeable e3) => GetFeatures (e1, e2, e3) where
    getFeatures (Entity ds) = case getFeatures (Entity ds) of
        Just (HCons e1 (HCons e2 (HCons e3 HNil))) -> Just (e1, e2, e3)
        Nothing -> Nothing

instance (Typeable e1, Typeable e2, Typeable e3, Typeable e4) => GetFeatures (e1, e2, e3, e4) where
    getFeatures (Entity ds) = case getFeatures (Entity ds) of
        Just (HCons e1 (HCons e2 (HCons e3 (HCons e4 HNil)))) -> Just (e1, e2, e3, e4)
        Nothing -> Nothing

instance (Typeable e1, Typeable e2, Typeable e3, Typeable e4, Typeable e5) => GetFeatures (e1, e2, e3, e4, e5) where
    getFeatures (Entity ds) = case getFeatures (Entity ds) of
        Just (HCons e1 (HCons e2 (HCons e3 (HCons e4 (HCons e5 HNil))))) -> Just (e1, e2, e3, e4, e5)
        Nothing -> Nothing

instance (Typeable e1, Typeable e2, Typeable e3, Typeable e4, Typeable e5, Typeable e6) => GetFeatures (e1, e2, e3, e4, e5, e6) where
    getFeatures (Entity ds) = case getFeatures (Entity ds) of
        Just (HCons e1 (HCons e2 (HCons e3 (HCons e4 (HCons e5 (HCons e6 HNil)))))) -> Just (e1, e2, e3, e4, e5, e6)
        Nothing -> Nothing

instance (Typeable e1, Typeable e2, Typeable e3, Typeable e4, Typeable e5, Typeable e6, Typeable e7) => GetFeatures (e1, e2, e3, e4, e5, e6, e7) where
    getFeatures (Entity ds) = case getFeatures (Entity ds) of
        Just (HCons e1 (HCons e2 (HCons e3 (HCons e4 (HCons e5 (HCons e6 (HCons e7 HNil))))))) -> Just (e1, e2, e3, e4, e5, e6, e7)
        Nothing -> Nothing

