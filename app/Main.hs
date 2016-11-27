module Main where

--import Lib
--import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor)
--import System.Environment
import  Examples.ShoeLift.GeorgeSandalls(generateTreadCubesToCxForErrors, generateAllTreadStl, generateRearTreadStl, generateForwardTreadStl)

main :: IO ()
main = do
 --x <-  getArgs
 --loadMDRAndPassToProcessor (read $ head x) (read $ head $ tail x)

 --crazyBB boot lift
 
 --generateTreadCubesToCxForErrors []
  
  generateForwardTreadStl 
 
 
