module Main where

--import Lib
--import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor)
--import System.Environment

import Examples.Diffs.MTLDiff(generateSingleLargeCubeToCxForErrors, generateSingleLargeCubeStl,
                              generatecutterCubesToCxForErrors, generatecutterCubesStl,
                              generateUnionCubesToCxForErrors, generateUnionCubesStl)

main :: IO ()
main = do
 --x <-  getArgs
 --loadMDRAndPassToProcessor (read $ head x) (read $ head $ tail x)

 
 generateUnionCubesStl
 --generateUnionCubesToCxForErrors
 
