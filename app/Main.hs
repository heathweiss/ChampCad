module Main where

--import Lib
--import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor, socketWithRiserStlGenerator)
import Examples.OpenBionicsCom.OpenBionicsDotComDesignWork (fullLengthSocketWithSmallShaftStlGenerator, wristToSmallShaftStlGenerator,
                                                  topOfSocketStlGenerator, joinerShaftStlGenerator,
                                                  shortSocketToLargeShaftStlGenerator, wristToLargeShaftStlGenerator
                                                  )
import Examples.OpenHand.Wrist(wristAndDoubleCylinderStlGenerator, wristAndDoubleCylinderShowCubes, wristSquaredOffStlGenerator, wristSquaredOffShowCubes)
import Examples.OpenHand.SocketMount(socketMountStlGenerator, socketMountShowCubes, socketMountTestsDo, generateSocketMountStlUsingDbValues,
                                     initializeDatabase, insertMount, viewMountByName, setCurrentMount )
import Examples.OpenHand.MotorMount(motorMountStlGenerator, motorMountShowCubes)
import Examples.OpenHand.FlexiSocket(flexiSocketTestsDo, flexSocketStlGenerator, testCubeStlGenerator, testCubeShowCubes)

import Examples.Primitives.Squared(cylinderWithSquaredRadiiStlGenerator, )

import Examples.Primitives.Cylinders(slopedToppedCylinder, solidCylinderSquared, solidCylinderLengthenY, solidCylinderSquared, walledCylinderSquared)

import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor, )

import Examples.Primitives.Cube(writeJoinerStlFile)

import Examples.Diffs.MTLDiff(generateUnionCubesStl)

import Examples.ShoeLift.SnowBoardBoot (writeRearSlopedTread, writeRearBoot, writeForwardBoot, writeRearRoundedToeTread)

--import System.Environment


--let type of Main to be inferred for running embedded tests
-- main :: IO ()
--main :: IO (Key CurrentMount)
main = do
 --x <-  getArgs
 --loadMDRAndPassToProcessor (read $ head x) (read $ head $ tail x)

  testCubeStlGenerator
  --testCubeShowCubes
  --flexSocketStlGenerator
  --flexiSocketTestsDo
  
  


