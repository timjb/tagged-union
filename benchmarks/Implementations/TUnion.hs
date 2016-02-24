{-# LANGUAGE DataKinds #-}

module Implementations.TUnion where

import Data.TaggedUnion

import Values
import Data.HVect

type L = '[A, B, C, D, E, F, G, H]
type S = TUnion L

lProxy :: Proxy L
lProxy = Proxy

sa, sb, sc, sd, se, sf, sg, sh :: S
sa = inject va lProxy
sb = inject vb lProxy
sc = inject vc lProxy
sd = inject vd lProxy
se = inject ve lProxy
sf = inject vf lProxy
sg = inject vg lProxy
sh = inject vh lProxy


elimS :: S -> Int
elimS = elim (elimA :&: elimB :&: elimC :&: elimD :&: elimE :&: elimF :&: elimG :&: elimH :&: HNil)