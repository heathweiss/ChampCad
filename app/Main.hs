module Main where

--import Lib
--import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor)
--import System.Environment
import  Examples.ShoeLift.CrazyBBoots(generateCutTreadRearStl, generateCutTreadFrontStl, showCutTreadCubesState,
                                      generateBootTreadRearStl, generateBootTreadFrontStl, showBootTreadCubesState)

main :: IO ()
main = do
 --x <-  getArgs
 --loadMDRAndPassToProcessor (read $ head x) (read $ head $ tail x)

 --crazyBB boot lift
 --generateCutTreadRearStl
 generateCutTreadFrontStl
 --showCutTreadCubesState []

 --showBootTreadCubesState []
 --generateBootTreadFrontStl
 --generateBootTreadRearStl
 
