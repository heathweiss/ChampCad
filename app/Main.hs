module Main where

--import Lib
--import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor)
--import System.Environment
import  Examples.ShoeLift.CrazyBBoots(generateHammerHeadSharkHeadSectionStl, showHammerHeadSharkHeadSectionCumulativeCornerPoints)

main :: IO ()
main = do
 --x <-  getArgs
 --loadMDRAndPassToProcessor (read $ head x) (read $ head $ tail x)
 generateHammerHeadSharkHeadSectionStl []
 --showHammerHeadSharkHeadSectionCumulativeCornerPoints []
