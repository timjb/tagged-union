{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Data.TaggedUnion.Internal
  ( TUnion (..)
  ) where

import GHC.Prim (Any (..))

data TUnion :: [*] -> * where
  TUnion :: !Int -> !Any -> TUnion ts