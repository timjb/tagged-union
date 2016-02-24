{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.TaggedUnion

import Test.Framework
import qualified Data.Char
import Data.HVect (HVect (..))
import qualified Control.Lens as L

main :: IO ()
main = htfMain htf_thisModulesTests

type L = '[Int, Char, Bool]

lProxy :: Proxy L
lProxy = Proxy

someInt, someChar, someBool :: TUnion L
someInt = inject (3 :: Int) lProxy
someChar = inject 'b' lProxy
someBool = inject True lProxy

test_intFromSomeInt   = assertEqual (Just 3)    (project (Proxy :: Proxy Int)  someInt)
test_charFromSomeInt  = assertEqual Nothing     (project (Proxy :: Proxy Char) someInt)
test_boolFromSomeInt  = assertEqual Nothing     (project (Proxy :: Proxy Bool) someInt)
test_intFromSomeChar  = assertEqual Nothing     (project (Proxy :: Proxy Int)  someChar)
test_charFromSomeChar = assertEqual (Just 'b')  (project (Proxy :: Proxy Char) someChar)
test_boolFromSomeChar = assertEqual Nothing     (project (Proxy :: Proxy Bool) someChar)
test_intFromSomeBool  = assertEqual Nothing     (project (Proxy :: Proxy Int)  someBool)
test_charFromSomeBool = assertEqual Nothing     (project (Proxy :: Proxy Char) someBool)
test_boolFromSomeBool = assertEqual (Just True) (project (Proxy :: Proxy Bool) someBool)

intElim :: Int -> Int
intElim = id

charElim :: Char -> Int
charElim = Data.Char.ord

boolElim :: Bool -> Int
boolElim b = if b then 1 else 0

lPrism :: Elem x L => L.Prism' (TUnion L) x
lPrism = unionPrism lProxy

test_prismSomeInt = assertEqual (Just (4 :: Int)) ((L.over lPrism ((+1) :: Int -> Int) someInt) L.^? lPrism)

test_prismSomeChar = assertEqual (Nothing :: Maybe Int) (someChar L.^? lPrism)

lElim = elim (intElim :&: charElim :&: boolElim :&: HNil)

test_elimSomeInt  = assertEqual 3  (lElim someInt)
test_elimSomeChar = assertEqual 98 (lElim someChar)
test_elimSomeBool = assertEqual 1  (lElim someBool)