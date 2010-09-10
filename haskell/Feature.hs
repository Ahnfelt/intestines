{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances  #-}
-- NOTE: UndecidableInstances is required by the Combine type class,
--       which is a restricted copy of HAppend in the HList module.
--       It can be removed yielding a slightly less precise system.
--       It is also used for the Has type class synonym.

module Feature ((.:.), Combine ((+++)), Entity, toEntity, getFeature, getFeatures, Has, has, nil) where

import Data.HList
import Data.Dynamic
import Data.Maybe


newtype Entity = Entity [Dynamic]


infixr 6 .:.
(.:.) :: (HOccursNot e l, HExtend e l l') => e -> l -> l'
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

instance Typeable a => Apply ToDynamic a Dynamic where 
    apply ToDynamic = toDyn

toEntity l = Entity $ hMapOut ToDynamic l


getFeature :: Typeable e => Entity -> Maybe e
getFeature (Entity ds) = case catMaybes $ map fromDynamic ds of
    e:_ -> Just e
    [] -> Nothing

getFeatures :: GetFeatures l => Entity -> Maybe l
getFeatures = getFeatures'

class GetFeatures l where
    getFeatures' :: Entity -> Maybe l

instance GetFeatures HNil where
    getFeatures' _ = Just HNil

instance (GetFeatures l, Typeable e) => GetFeatures (HCons e l) where
    getFeatures' (Entity ds) = case catMaybes $ map fromDynamic ds of
        e:_ -> case getFeatures' (Entity ds) of
            Just l -> Just (HCons e l)
            Nothing -> Nothing
        [] -> Nothing

instance (Typeable e1, Typeable e2) => GetFeatures (e1, e2) where
    getFeatures' (Entity ds) = case getFeatures' (Entity ds) of
        Just (HCons e1 (HCons e2 HNil)) -> Just (e1, e2)
        Nothing -> Nothing

instance (Typeable e1, Typeable e2, Typeable e3) => GetFeatures (e1, e2, e3) where
    getFeatures' (Entity ds) = case getFeatures' (Entity ds) of
        Just (HCons e1 (HCons e2 (HCons e3 HNil))) -> Just (e1, e2, e3)
        Nothing -> Nothing

instance (Typeable e1, Typeable e2, Typeable e3, Typeable e4) => GetFeatures (e1, e2, e3, e4) where
    getFeatures' (Entity ds) = case getFeatures' (Entity ds) of
        Just (HCons e1 (HCons e2 (HCons e3 (HCons e4 HNil)))) -> Just (e1, e2, e3, e4)
        Nothing -> Nothing

instance (Typeable e1, Typeable e2, Typeable e3, Typeable e4, Typeable e5) => GetFeatures (e1, e2, e3, e4, e5) where
    getFeatures' (Entity ds) = case getFeatures' (Entity ds) of
        Just (HCons e1 (HCons e2 (HCons e3 (HCons e4 (HCons e5 HNil))))) -> Just (e1, e2, e3, e4, e5)
        Nothing -> Nothing

instance (Typeable e1, Typeable e2, Typeable e3, Typeable e4, Typeable e5, Typeable e6) => GetFeatures (e1, e2, e3, e4, e5, e6) where
    getFeatures' (Entity ds) = case getFeatures' (Entity ds) of
        Just (HCons e1 (HCons e2 (HCons e3 (HCons e4 (HCons e5 (HCons e6 HNil)))))) -> Just (e1, e2, e3, e4, e5, e6)
        Nothing -> Nothing

instance (Typeable e1, Typeable e2, Typeable e3, Typeable e4, Typeable e5, Typeable e6, Typeable e7) => GetFeatures (e1, e2, e3, e4, e5, e6, e7) where
    getFeatures' (Entity ds) = case getFeatures' (Entity ds) of
        Just (HCons e1 (HCons e2 (HCons e3 (HCons e4 (HCons e5 (HCons e6 (HCons e7 HNil))))))) -> Just (e1, e2, e3, e4, e5, e6, e7)
        Nothing -> Nothing

