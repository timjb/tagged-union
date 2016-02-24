module Main where

import Criterion.Main

import qualified Implementations.BalancedEithers as BE
import qualified Implementations.LopsidedEithers as LE
import qualified Implementations.Sumtype as ST
import qualified Implementations.TUnion as TU

main :: IO ()
main = defaultMain [
  bgroup "sum"
    [ bench "BalancedEithers"  $ whnf (sum . map BE.elimS) [BE.sa, BE.sb, BE.sc, BE.sd, BE.se, BE.sf, BE.sg, BE.sh]
    , bench "LopsidedEithers"  $ whnf (sum . map LE.elimS) [LE.sa, LE.sb, LE.sc, LE.sd, LE.se, LE.sf, LE.sg, LE.sh]
    , bench "Sumtype"          $ whnf (sum . map ST.elimS) [ST.sa, ST.sb, ST.sc, ST.sd, ST.se, ST.sf, ST.sg, ST.sh]
    , bench "TUnion"           $ whnf (sum . map TU.elimS) [TU.sa, TU.sb, TU.sc, TU.sd, TU.se, TU.sf, TU.sg, TU.sh]
    ]
  ]
