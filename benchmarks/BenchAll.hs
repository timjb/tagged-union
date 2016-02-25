module Main where

import Criterion.Main

import qualified Implementations.BalancedEithers as BE
import qualified Implementations.StrictBalancedEithers as SBE
import qualified Implementations.LopsidedEithers as LE
import qualified Implementations.StrictLopsidedEithers as SLE
import qualified Implementations.Sumtype as ST
import qualified Implementations.StrictSumtype as SST
import qualified Implementations.TUnion as TU

main :: IO ()
main = defaultMain [
  bgroup "sum"
    [ bench "BalancedEithers" $ whnf (sum . map BE.elimS) [BE.sa, BE.sb, BE.sc, BE.sd, BE.se, BE.sf, BE.sg, BE.sh]
    , bench "StrictBalancedEithers" $ whnf (sum . map SBE.elimS) [SBE.sa, SBE.sb, SBE.sc, SBE.sd, SBE.se, SBE.sf, SBE.sg, SBE.sh]
    , bench "LopsidedEithers" $ whnf (sum . map LE.elimS) [LE.sa, LE.sb, LE.sc, LE.sd, LE.se, LE.sf, LE.sg, LE.sh]
    , bench "StrictLopsidedEithers" $ whnf (sum . map SLE.elimS) [SLE.sa, SLE.sb, SLE.sc, SLE.sd, SLE.se, SLE.sf, SLE.sg, SLE.sh]
    , bench "Sumtype" $ whnf (sum . map ST.elimS) [ST.sa, ST.sb, ST.sc, ST.sd, ST.se, ST.sf, ST.sg, ST.sh]
    , bench "StrictSumtype" $ whnf (sum . map SST.elimS) [SST.sa, SST.sb, SST.sc, SST.sd, SST.se, SST.sf, SST.sg, SST.sh]
    , bench "TUnion" $ whnf (sum . map TU.elimS) [TU.sa, TU.sb, TU.sc, TU.sd, TU.se, TU.sf, TU.sg, TU.sh]
    ]
  ]
