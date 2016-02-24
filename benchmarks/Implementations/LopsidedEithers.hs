module Implementations.LopsidedEithers where

import Values

type S = Either A (Either B (Either C (Either D (Either E (Either F (Either G H))))))

sa, sb, sc, sd, se, sf, sg, sh :: S
sa = Left va
sb = Right (Left vb)
sc = Right (Right (Left vc))
sd = Right (Right (Right (Left vd)))
se = Right (Right (Right (Right (Left ve))))
sf = Right (Right (Right (Right (Right (Left vf)))))
sg = Right (Right (Right (Right (Right (Right (Left vg))))))
sh = Right (Right (Right (Right (Right (Right (Right vh))))))

elimS :: S -> Int
elimS = either elimA (either elimB (either elimC (either elimD (either elimE (either elimF (either elimG elimH))))))
