{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Data.TaggedUnion
  ( module Data.TaggedUnion.TypeLevel
  , TUnion
  , inject
  , project
  , unionPrism
  , elim
  ) where

import Data.TaggedUnion.Internal
import Data.TaggedUnion.TypeLevel

import Unsafe.Coerce (unsafeCoerce)
import Data.Profunctor (Profunctor (..), Choice (..))
import Data.HVect (HVect (..))
import GHC.Prim (Any (..))
import qualified Data.Vector as V

inject :: Elem x xs => x -> Proxy xs -> TUnion xs
inject (v :: x) xsProxy =
  let i = elemIndex (Proxy :: Proxy x) xsProxy
  in TUnion i (unsafeCoerce v)
{-# INLINE inject #-}

project :: Elem x xs => Proxy x -> TUnion xs -> Maybe x
project (Proxy :: Proxy x) (TUnion i v :: TUnion xs) =
  let j = elemIndex (Proxy :: Proxy x) (Proxy :: Proxy xs)
  in if i == j then Just (unsafeCoerce v) else Nothing

unionPrism
  :: forall p f x xs. (Choice p, Applicative f, Elem x xs)
  => Proxy xs
  -> p x (f x) -> p (TUnion xs) (f (TUnion xs))
unionPrism xsProxy (g :: p x (f x)) =
  prism' (flip inject xsProxy) (project (Proxy :: Proxy x)) g
  where
    prism bt seta = dimap seta (either pure (fmap bt)) . right'
    prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))

unsafeCoerceAll :: HVect as -> [b]
unsafeCoerceAll HNil = []
unsafeCoerceAll (f :&: fs) = unsafeCoerce f : unsafeCoerceAll fs
{-# INLINE unsafeCoerceAll #-}

elimWithFunctionTable :: V.Vector (Any -> a) -> TUnion xs -> a
elimWithFunctionTable functionTable (TUnion i v) =
  (V.unsafeIndex functionTable i) v
{-# INLINE elimWithFunctionTable #-}

elim :: HVect (Elims xs a) -> TUnion xs -> a
elim eliminators =
  elimWithFunctionTable (V.fromList (unsafeCoerceAll eliminators))
{-# INLINE elim #-}
