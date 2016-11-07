module Main where

--import Lib
--import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor)
--import System.Environment
import  Examples.ShoeLift.CrazyBBoots(generateCutTreadFrontStlV2, generateCutTreadRearStlV2,
                                      generateBootTreadRearStl, generateBootTreadFrontStl, showBootTreadCubesState,
                                      generateCutTreadTestFitRearStl)

main :: IO ()
main = do
 --x <-  getArgs
 --loadMDRAndPassToProcessor (read $ head x) (read $ head $ tail x)

 --crazyBB boot lift
 generateCutTreadTestFitRearStl

 --showBootTreadCubesState []
 --generateBootTreadFrontStl
 --generateBootTreadRearStl
 
