module Values where

import qualified Data.Char

type A = Int
type B = Char
type C = Bool
type D = (Int, Int)
type E = Float
type F = ()
type G = Int -> Int
type H = Either Int Char

va :: A
va = 3

vb :: B
vb = 'b'

vc :: C
vc = True

vd :: D
vd = (99, 100)

ve :: E
ve = 13.37

vf :: F
vf = ()

vg :: G
vg = (*4)

vh :: H
vh = Left 5

elimA :: A -> Int
elimA = id

elimB :: B -> Int
elimB = Data.Char.ord

elimC :: C -> Int
elimC b = if b then 1 else 0

elimD :: D -> Int
elimD (x, y) = x - y

elimE :: E -> Int
elimE = floor

elimF :: F -> Int
elimF () = 42

elimG :: G -> Int
elimG g = g 23

elimH :: H -> Int
elimH = either elimA elimB
