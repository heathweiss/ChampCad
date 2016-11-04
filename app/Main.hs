module Main where

--import Lib
--import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor)
--import System.Environment
import  Examples.ShoeLift.CrazyBBoots(generateCutTreadFrontStlV2,
                                      generateBootTreadRearStl, generateBootTreadFrontStl, showBootTreadCubesState)

main :: IO ()
main = do
 --x <-  getArgs
 --loadMDRAndPassToProcessor (read $ head x) (read $ head $ tail x)

 --crazyBB boot lift
 --generateCutTreadRearStl
 generateCutTreadFrontStlV2
 --showCutTreadCubesState []

 --showBootTreadCubesState []
 --generateBootTreadFrontStl
 --generateBootTreadRearStl
 
