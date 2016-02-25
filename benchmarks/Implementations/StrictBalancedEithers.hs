module Implementations.StrictBalancedEithers where

import Values
import StrictEither

type S =
  Either'
    (Either'
      (Either' A B)
      (Either' C D))
    (Either'
      (Either' E F)
      (Either' G H))

sa, sb, sc, sd, se, sf, sg, sh :: S
sa = Left' (Left' (Left' va))
sb = Left' (Left' (Right' vb))
sc = Left' (Right' (Left' vc))
sd = Left' (Right' (Right' vd))
se = Right' (Left' (Left' ve))
sf = Right' (Left' (Right' vf))
sg = Right' (Right' (Left' vg))
sh = Right' (Right' (Right' vh))

elimS :: S -> Int
elimS =
  either'
    (either'
      (either' elimA elimB)
      (either' elimC elimD))
    (either'
      (either' elimE elimF)
      (either' elimG elimH))