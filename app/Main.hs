module Main where

--import Lib
--import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor, socketWithRiserStlGenerator)
import Examples.OpenBionicsCom.OpenBionicsDotComDesignWork (fullLengthSocketWithSmallShaftStlGenerator, handtoTriacontakaihexagonStlGenerator,
                                                  topOfSocketStlGenerator, triacontakaihexagonInnerRadiiShaftStlGenerator,
                                                  shortSocketToLargeShaftStlGenerator, wristToLargeShaftStlGenerator
                                                  )
--import System.Environment



main :: IO ()
main = do
 --x <-  getArgs
 --loadMDRAndPassToProcessor (read $ head x) (read $ head $ tail x)

 
  wristToLargeShaftStlGenerator
  
  
  
