module Main where

--import Lib
--import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor, socketWithRiserStlGenerator)
import Examples.OpenBionicsCom.OpenBionicsDotComDesignWork (fullLengthSocketWithSmallShaftStlGenerator, wristToSmallShaftStlGenerator,
                                                  topOfSocketStlGenerator, joinerShaftStlGenerator,
                                                  shortSocketToLargeShaftStlGenerator, wristToLargeShaftStlGenerator
                                                  )
import Examples.Primitives.Squared(cylinderWithSquaredRadiiStlGenerator)

import Examples.Primitives.ComposableExample(createCylinderComposableSlopedStlGenerator, createCylinderComposableSlopedCumulativeCornerPoints,
                                            createDoubleCylinderComposableCumulativeCornerPoints, createDoubleCylinderComposableStlGenerator,
                                            createDoubleCylinderSquaredStlGenerator, createDoubleCylinderSquaredCumulativeCornerPoints,
                                            createDoubleCylinderSquaredAndSlopedStlGenerator, createDoubleCylinderSquaredAndSlopedCumulativeCornerPoints)
import Examples.Primitives.Cylinders(slopedToppedCylinder, solidCylinderSquared, solidCylinderLengthenY, solidCylinderSquared, walledCylinderSquared)
--import System.Environment



main :: IO ()
main = do
 --x <-  getArgs
 --loadMDRAndPassToProcessor (read $ head x) (read $ head $ tail x)

 
  walledCylinderSquared
  
  
  
  
  
