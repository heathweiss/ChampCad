module Main where

--import Lib
--import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor, socketWithRiserStlGenerator)
import Examples.OpenBionicsCom.OpenBionicsDotComDesignWork (fullLengthSocketWithSmallShaftStlGenerator, wristToSmallShaftStlGenerator,
                                                  topOfSocketStlGenerator, joinerShaftStlGenerator,
                                                 shortSocketToLargeShaftStlGenerator, wristToLargeShaftStlGenerator
                                                  )
import qualified  Examples.OpenHand.Wrist as W (wristAndDoubleCylinderStlGenerator, wristAndDoubleCylinderShowCubes,
                               initializeDatabase, insertWristDimensions, wristWithRoundRiserDBGenerator)
import qualified Examples.OpenHand.SocketMount as SM (socketMountTestsDo, 
                                     initializeDatabase, insertMount, setCurrentMount, showFaceDimensions,
                                     generateSocketMountWithDegreesStlUsingDbValues)
import qualified Examples.OpenHand.MotorMount as M (motorMountHardCodedStlGenerator, motorMountHardCodedShowCubes,
                                   initializeDatabase, insertMotorMount, motorMountRunGeneratorFromDB)
import qualified Examples.OpenHand.FlexiSocket as Flex (flexSocketStlGenerator, flexSocketPlainStlGenerator, flexSocketPlainStlGeneratorDbStlGeneretor,
                                     initializeDatabase, insertFlexDimensions, flexSocketWithRiserDbStlGenerator, flexSocketShowCurrentState,
                                     flexBottomForSocketWithRiserDbStlGenerator)
import qualified Examples.OpenHand.Common  as C (initializeDatabase, insertDimensions, seeCommonFactors)
import qualified Examples.OpenHand.FingerJoints as FJnts(fingerJointsStlGenerator, fingerJointsToRiserStlGenerator)

import Examples.Primitives.Squared(cylinderWithSquaredRadiiStlGenerator, )

import Examples.Primitives.Cylinders(slopedToppedCylinder, solidCylinderLengthenY, solidCylinderSquared, walledCylinderSquared)

import Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor)

import Examples.Primitives.Cube(writeJoinerStlFile)

import Examples.Diffs.MTLDiff(generateUnionCubesStl)

import Examples.ShoeLift.SnowBoardBoot (writeRearSlopedTread, writeRearBoot, writeForwardBoot, writeRearRoundedToeTread)

import System.Environment


--let type of Main to be inferred for running embedded tests
-- main :: IO ()
--main :: IO (Key CurrentMount)
main = do
  let currentSocket = "sharkfin" -- "mount 1" --
  -- x <-  getArgs
 --loadMDRAndPassToProcessor (read $ head x) (read $ head $ tail x)

  SM.generateSocketMountWithDegreesStlUsingDbValues currentSocket
  --SM.showSocketMountCubesUsingDbValues currentSocket
  --SM.socketMountTestsDo

  --flexSocketPlainStlGeneratorDbStlGeneretor (head x)
  --wristSquaredOffStlFromDbGenerator
  --generateSocketMountStlUsingDbValues
  --M.initializeDatabase
  --M.insertMotorMount
  --M.motorMountRunGeneratorFromDB (head x)
  --M.motorMountRunGeneratorFromDB currentSocket
  
  --FJnts.fingerJointsToRiserStlGenerator

  --Flex.flexBottomForSocketWithRiserDbStlGenerator currentSocket
  --Flex.flexSocketWithRiserDbStlGenerator currentSocket
  --Flex.insertFlexDimensions
  --Flex.flexiSocketTestsDo
  --Flex.flexSocketStlGenerator
  --Flex.flexSocketShowCurrentState
  
  --W.insertWristDimensions --
  --W.initializeDatabase
  --W.wristWithRoundRiserDBGenerator currentSocket

  
  --solidCylinderSquared
