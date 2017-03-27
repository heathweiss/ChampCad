module Main where

--import Lib
--import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor, socketWithRiserStlGenerator)
import Examples.OpenBionicsCom.OpenBionicsDotComDesignWork (fullLengthSocketWithSmallShaftStlGenerator, wristToSmallShaftStlGenerator,
                                                  topOfSocketStlGenerator, joinerShaftStlGenerator,
                                                 shortSocketToLargeShaftStlGenerator, wristToLargeShaftStlGenerator
                                                  )
import qualified  Examples.OpenHand.Wrist as W (wristAndDoubleCylinderStlGenerator, wristAndDoubleCylinderShowCubes,
                               wristSquaredOffStlGenerator, wristSquaredOffShowCubes, wristSquaredOffStlFromDbGenerator,
                               initializeDatabase, insertWristDimensions, wristWithRoundRiserDBGenerator)
import Examples.OpenHand.SocketMount(socketMountStlGenerator, socketMountShowCubes, socketMountTestsDo, generateSocketMountStlUsingDbValues,
                                     initializeDatabase, insertMount, viewMountByName, setCurrentMount, showFaceDimensions )
import qualified Examples.OpenHand.MotorMount as M (motorMountHardCodedStlGenerator, motorMountHardCodedShowCubes,
                                   initializeDatabase, insertMotorMount, motorMountRunGeneratorFromDB)
import qualified Examples.OpenHand.FlexiSocket as Flex (flexiSocketTestsDo, flexSocketStlGenerator, testCubeStlGenerator, testCubeShowCubes,
                                     testCubeRotatedStlGenerator, flexSocketPlainStlGenerator, flexSocketPlainStlGeneratorDbStlGeneretor,
                                     initializeDatabase, insertFlexDimensions, flexSocketWithRiserDbStlGenerator)
import qualified Examples.OpenHand.Common  as C (initializeDatabase, insertDimensions, seeCommonFactors)

import Examples.Primitives.Squared(cylinderWithSquaredRadiiStlGenerator, )

import Examples.Primitives.Cylinders(slopedToppedCylinder, solidCylinderSquared, solidCylinderLengthenY, solidCylinderSquared, walledCylinderSquared)

import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor)

import Examples.Primitives.Cube(writeJoinerStlFile)

import Examples.Diffs.MTLDiff(generateUnionCubesStl)

import Examples.ShoeLift.SnowBoardBoot (writeRearSlopedTread, writeRearBoot, writeForwardBoot, writeRearRoundedToeTread)

import System.Environment


--let type of Main to be inferred for running embedded tests
-- main :: IO ()
--main :: IO (Key CurrentMount)
main = do
  let currentSocket = "sharkfin"
  -- x <-  getArgs
 --loadMDRAndPassToProcessor (read $ head x) (read $ head $ tail x)

  --flexSocketPlainStlGeneratorDbStlGeneretor (head x)
  --wristSquaredOffStlFromDbGenerator
  --generateSocketMountStlUsingDbValues
  --M.motorMountRunGeneratorFromDB (head x)
  --M.motorMountRunGeneratorFromDB currentSocket
  --C.insertDimensions
  --Flex.flexSocketWithRiserDbStlGenerator currentSocket
  --Flex.insertFlexDimensions
  --W.insertWristDimensions --
  --W.initializeDatabase
  W.wristWithRoundRiserDBGenerator currentSocket
