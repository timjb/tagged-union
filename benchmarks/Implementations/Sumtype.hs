module Implementations.Sumtype where

import Values

data S = SA A | SB B | SC C | SD D | SE E | SF F | SG G | SH H

sa, sb, sc, sd, se, sf, sg, sh :: S
sa = SA va
sb = SB vb
sc = SC vc
sd = SD vd
se = SE ve
sf = SF vf
sg = SG vg
sh = SH vh

elimS :: S -> Int
elimS (SA a) = elimA a
elimS (SB b) = elimB b
elimS (SC c) = elimC c
elimS (SD d) = elimD d
elimS (SE e) = elimE e
elimS (SF f) = elimF f
elimS (SG g) = elimG g
elimS (SH h) = elimH h