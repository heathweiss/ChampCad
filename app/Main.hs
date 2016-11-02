module Main where

--import Lib
--import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor)
--import System.Environment
import  Examples.ShoeLift.CrazyBBoots(generateRearStl, generateFrontStl, showLowerCubesState)

main :: IO ()
main = do
 --x <-  getArgs
 --loadMDRAndPassToProcessor (read $ head x) (read $ head $ tail x)

 --crazyBB boot lift
 generateRearStl
 --generateFrontStl
 --showLowerCubesState []
 
