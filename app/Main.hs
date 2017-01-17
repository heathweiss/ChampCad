module Main where

--import Lib
import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor, socketWithRiserStlGenerator)
import Examples.Scan.OpenBionicsDotComDesignWork (socketNoConnectorStlGenerator, handBaseStlGenerator)
--import System.Environment



main :: IO ()
main = do
 --x <-  getArgs
 --loadMDRAndPassToProcessor (read $ head x) (read $ head $ tail x)

 
 

 handBaseStlGenerator 
 
