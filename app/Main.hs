module Main where

--import Lib
--import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor)
--import System.Environment
import  Examples.ShoeLift.CrazyBBoots(generateCutTreadBottomStl, showCutTreadBottomCumulativeCornerPoints)

main :: IO ()
main = do
 --x <-  getArgs
 --loadMDRAndPassToProcessor (read $ head x) (read $ head $ tail x)

 --crazyBB boot lift
 generateCutTreadBottomStl []
 --showCutTreadBottomCumulativeCornerPoints []
 
