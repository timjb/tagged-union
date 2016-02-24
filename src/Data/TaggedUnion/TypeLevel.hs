{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TaggedUnion.TypeLevel
  ( Proxy (..)
  , Elem (..)
  , Elims
  ) where

import GHC.Exts (Constraint)

data Proxy (t :: k) = Proxy

-- Stolen from Jon Sterling
-- https://github.com/VinylRecords/Vinyl/pull/64
class ErrMsg (err :: k) where
  neverEver :: forall f a. f err -> a

type family (b :: Bool) ?? (err :: k) :: Constraint where
  True ?? err = ()
  False ?? err = ErrMsg err

type family NotEqB (a :: k) (b :: k) where
  NotEqB x x = False
  NotEqB x y = True

type NotEq x y = NotEqB x y ?? '(x, "should not be equal to", y)

class Elem (x :: k) (xs :: [k]) where
  elemIndex :: Proxy x -> Proxy xs -> Int

instance {-# OVERLAPPING #-} Elem x (x ': xs) where
  elemIndex _ _ = 0

instance {-# OVERLAPPABLE #-} (NotEq x y, Elem x xs) => Elem x (y ': xs) where
  elemIndex proxy (Proxy :: Proxy (y ': xs)) =
    1 + elemIndex proxy (Proxy :: Proxy xs)

type family Elims (xs :: [*]) (a :: *) :: [*] where
  Elims '[] a = '[]
  Elims (x ': xs) a = (x -> a) ': (Elims xs a)
  

{-
type family ElemB (x :: k) (xs :: [k]) :: Bool where
  ElemB x '[]       = False
  ElemB x (x ': xs) = True
  ElemB y (x ': xs) = ElemB y xs

type Elem x xs = ElemB x xs ?? '(x, " is not an element of ", xs)
-}
