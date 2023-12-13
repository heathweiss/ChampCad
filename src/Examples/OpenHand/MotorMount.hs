--for persist
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
The container into which the motors and board will be mounted.
-}

module Examples.OpenHand.MotorMount(motorMountHardCodedStlGenerator, motorMountHardCodedShowCubes,
                                   initializeDatabase, insertMotorMount, motorMountRunGeneratorFromDB) where

-- for persist
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Persistable.Base as PstB

import Data.Text hiding (map)

import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsList,
                     CpointsStack, CpointsList, buildCubePointsListWithAdd, buildCubePointsListSingle)

import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|), (+++>))
import CornerPoints.Points(Point(..))
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace )
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)
import CornerPoints.MeshGeneration(autoGenerateEachCube)
import CornerPoints.FaceExtraction(extractRightFace, extractFrontFace, extractTopFace)


import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader

import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))

import Control.Lens
-- ============================ database values motor mount==============================================================
-- ======================================================================================================================
-- ======================================================================================================================
databaseName = "src/Examples/OpenHand/MotorMount.sql" :: Text

share [mkPersist sqlSettings { mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistLowerCase|
 MotorMount
   name String
   UniqueName name
   desc String
   leftWall Double
   leftSeal Double
   x1Width  Double
   x2Width  Double
   x3Width Double
   x4Width  Double
   x5Width  Double
   x6Width  Double
   x7Width  Double
   x8Width  Double
   x9Width  Double
   x10Width  Double
   center Double
   x11Width Double
   x12Width Double
   x13Width Double
   x14Width Double
   x15Width Double
   x16Width Double
   x17Width Double
   x18Width Double
   x19Width Double
   x20Width Double
   rightSeal Double
   rightWall Double

   backWallWidth Double
   backSealWidth Double
   boardLength Double
   motorLength Double
   frontSealWidth Double
   frontWallWidth Double

   z1Height Double
   z2Height Double
   z3Height Double
   z4Height Double
  deriving Show


 |]

-- | Initialize a new database with all tables. Will alter tables of existing db.
initializeDatabase :: IO ()
initializeDatabase = runSqlite databaseName . PstB.asSqlBackendReader $ do
       
    runMigration migrateAll
    liftIO $ putStrLn "db initializes"

-- | Insert a new Mount, FaceSlope, and FaceDimensions into the database. Sqlite browser will not do this.
insertMotorMount :: IO ()
insertMotorMount  = runSqlite databaseName . PstB.asSqlBackendReader $ do
  let stdXWidth = 2.5
      borderWidth = 2
      sealWidth = 2
      
  mountId <- insert $ MotorMount
    "mount 1"
    "the orginal print which was too narrow and tall"
    borderWidth --leftWall
    sealWidth --leftSeal
    stdXWidth --x1Width 
    stdXWidth --x2Width
    stdXWidth --x3Width 
    stdXWidth --x4Width  
    stdXWidth --x5Width
    stdXWidth --x6Width
    stdXWidth --x7Width
    stdXWidth --x8Width
    stdXWidth --x9Width
    stdXWidth --10Width 
    stdXWidth --center  
    stdXWidth --x11Width  
    stdXWidth --x12Width
    stdXWidth --x13Width 
    stdXWidth --x14Width
    stdXWidth --x15Width
    stdXWidth --x16Width
    stdXWidth --x17Width
    stdXWidth --x18Width
    stdXWidth --x19Width
    stdXWidth --x20Width
    sealWidth --rightSeal  
    borderWidth --rightWall

    borderWidth --backWallWidth
    sealWidth   --backSealWidth
    25          --boardLength
    40          --motorLength
    sealWidth   --frontSealWidth
    borderWidth --frontWallWidth

    0.2 --z1Height 
    17 --z2Height
    2 --z3Height
    2 --z4Height
    
  liftIO $ putStrLn "mount mount inserted"

motorMountRunGeneratorFromDB :: String -> IO ()
motorMountRunGeneratorFromDB mountName = runSqlite databaseName . PstB.asSqlBackendReader $ do
  maybeMount <- getBy $ UniqueName mountName
  case maybeMount of
    Nothing -> liftIO $ putStrLn "Just kidding, not really there"
    Just (Entity mountId mount) -> do
      liftIO $ putStrLn "generator loading"
      liftIO $ motorMountFromDbStlGenerator mount
      

motorMountFromDbStlGenerator :: MotorMount -> IO ()
motorMountFromDbStlGenerator motorMount  = do
  let cpoints =  ((execState $ runExceptT  $  motorMountFromDb  motorMount     ) [])
  writeStlToFile $ newStlShape "motorMountHardCoded"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

-- ============================================================= left off==========================================
-- Needs a set of solid walls on each end to make it solid. Could put a 3mm layer for this.
--Could also add a a 3mm shelf under the board using same 3mm layer. Would give lots of strenght.
--Needs more height at top of board. Could do this by lowing the motor heights by 2mm

motorMountFromDb :: MotorMount ->  ExceptT BuilderError (State CpointsStack ) CpointsList
--motorMountFromDb (MotorMount name desc x1Width x2Width x3Width x4Width x5Width ) = do
motorMountFromDb (MotorMount _ _
                             leftWall leftSeal
                             x1Width x2Width x3Width x4Width x5Width x6Width x7Width x8Width x9Width x10Width
                             centerWidth
                             x11Width x12Width x13Width x14Width x15Width x16Width x17Width x18Width x19Width x20Width
                             rightSeal rightWall
                             backWallWidth backSealWidth boardLength motorLength frontSealWidth frontWallWidth
                             z1Height z2Height z3Height z4Height )  = do
  
  backWallLeftWallBottomFaces
         <- buildCubePointsListSingle "y1x1BottomFaces"
            
             [
               ((B1 (Point 0 0 0)) +++ (B4 (Point (leftWall) 0 0)))
               +++
               --((F1 (Point 0 y1Width 0)) +++ (F4 (Point x1Width y1Width 0)))
               ((F1 (Point 0 (backWallWidth) 0)) +++ (F4 (Point (leftWall) (backWallWidth) 0)))
             ]
  backWallLeftWallCubes
      <- buildCubePointsListWithAdd "y1x1Cubes"
         (map ((transposeZ (+ (z1Height))) . upperFaceFromLowerFace) backWallLeftWallBottomFaces)
         backWallLeftWallBottomFaces

  backWallLeftSealCubes
     <- buildCubePointsListWithAdd "y1x2Cubes"
        (map ((transposeX (+(leftSeal))) . extractRightFace) backWallLeftWallCubes)
        (backWallLeftWallCubes)

  backWallx1Cubes
     <- buildCubePointsListWithAdd "y1x3Cubes"
        (map ((transposeX (+(x1Width))) . extractRightFace) backWallLeftSealCubes)
        (backWallLeftSealCubes)

  backWallx2Cubes
     <- buildCubePointsListWithAdd "y1x3Cubes"
        (map ((transposeX (+(x2Width))) . extractRightFace) backWallx1Cubes)
        (backWallx1Cubes)

  backWallx3Cubes
     <- buildCubePointsListWithAdd "y1x3Cubes"
        (map ((transposeX (+(x3Width))) . extractRightFace) backWallx2Cubes)
        (backWallx2Cubes)

  backWallx4Cubes
    <- buildCubePointsListWithAdd "y1x3Cubes"
       (map ((transposeX (+(x4Width))) . extractRightFace) backWallx3Cubes)
        (backWallx3Cubes)

  backWallx5Cubes
    <- buildCubePointsListWithAdd "y1x3Cubes"
       (map ((transposeX (+(x5Width))) . extractRightFace) backWallx4Cubes)
        (backWallx4Cubes)

  backWallx6Cubes
    <- buildCubePointsListWithAdd "y1x6Cubes"
       (map ((transposeX (+(x6Width))) . extractRightFace) backWallx5Cubes)
        (backWallx5Cubes)

  backWallx7Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(x7Width))) . extractRightFace) backWallx6Cubes)
        (backWallx6Cubes)

  backWallx8Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(x8Width))) . extractRightFace) backWallx7Cubes)
        (backWallx7Cubes)

  backWallx9Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(x9Width))) . extractRightFace) backWallx8Cubes)
        (backWallx8Cubes)

  backWallx10Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(x10Width))) . extractRightFace) backWallx9Cubes)
        (backWallx9Cubes)

  backWallCenterCubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(centerWidth))) . extractRightFace) backWallx10Cubes)
        (backWallx10Cubes)
  
  backWallx11Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(x11Width))) . extractRightFace) backWallCenterCubes)
        (backWallCenterCubes)
  
  backWallx12Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(x12Width))) . extractRightFace) backWallx11Cubes)
        (backWallx11Cubes)

  backWallx13Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(x13Width))) . extractRightFace) backWallx12Cubes)
        (backWallx12Cubes)

  backWallx14Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(x14Width))) . extractRightFace) backWallx13Cubes)
        (backWallx13Cubes)

  backWallx15Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(x15Width))) . extractRightFace) backWallx14Cubes)
        (backWallx14Cubes)
  
  backWallx16Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(x16Width))) . extractRightFace) backWallx15Cubes)
        (backWallx15Cubes)
  
  backWallx17Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(x17Width))) . extractRightFace) backWallx16Cubes)
        (backWallx16Cubes)

  backWallx18Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(x18Width))) . extractRightFace) backWallx17Cubes)
        (backWallx17Cubes)

  backWallx19Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(x19Width))) . extractRightFace) backWallx18Cubes)
        (backWallx18Cubes)

  backWallx20Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(x20Width))) . extractRightFace) backWallx19Cubes)
        (backWallx19Cubes)

  
  backWallRightSealCubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(rightSeal))) . extractRightFace) backWallx20Cubes)
        (backWallx20Cubes)

  backWallRightWallCubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(rightWall))) . extractRightFace) backWallRightSealCubes)
        (backWallRightSealCubes)

  let btmBackWallRow = backWallLeftWallCubes ++ backWallLeftSealCubes
              ++ backWallx1Cubes  ++ backWallx2Cubes ++ backWallx3Cubes ++ backWallx4Cubes ++ backWallx5Cubes  
              ++ backWallx6Cubes ++ backWallx7Cubes ++ backWallx8Cubes ++ backWallx9Cubes ++ backWallx10Cubes
              ++ backWallCenterCubes
              ++ backWallx11Cubes ++ backWallx12Cubes ++ backWallx13Cubes ++ backWallx14Cubes ++ backWallx15Cubes
              ++ backWallx16Cubes ++ backWallx17Cubes ++ backWallx18Cubes ++ backWallx19Cubes ++ backWallx20Cubes
              ++ backWallRightSealCubes ++ backWallRightWallCubes
  --y1 layer 1 done.
  --add rest of layer 1 y's by transposing along y axis
  btmBackSealRow <- buildCubePointsListWithAdd "btmBackSealRow"
             (btmBackWallRow)
             (map ((transposeY (+(backSealWidth))) . extractFrontFace) btmBackWallRow)

  btmBoardRow <- buildCubePointsListWithAdd "y2Layer"
             (btmBackSealRow)
             (map ((transposeY (+(boardLength))) . extractFrontFace) btmBackSealRow)

  btmMotorRow <- buildCubePointsListWithAdd "y2Layer"
             (btmBoardRow)
             (map ((transposeY (+(motorLength))) . extractFrontFace) btmBoardRow)
  
  btmFrontSealRow <- buildCubePointsListWithAdd "y2Layer"
             (btmMotorRow)
             (map ((transposeY (+(frontSealWidth))) . extractFrontFace) btmMotorRow)

             --borderWidth
  btmFrontWallRow <- buildCubePointsListWithAdd "y2Layer"
             (btmFrontSealRow)
             (map ((transposeY (+(frontWallWidth))) . extractFrontFace) btmFrontSealRow)


  -- ========== end of z layer 1(bottom layer)
  -- ========== start of layer 2
  let backWallBackSealCutters =
                [CornerPointsId, CornerPointsId, --left wall and seasl
                 CornerPointsId, CornerPointsId, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsId, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsNothing, --center
                 CornerPointsNothing, CornerPointsNothing, CornerPointsId, CornerPointsId, CornerPointsId,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsId,
                 CornerPointsId, CornerPointsId --right wall and seasl                          
                ]
  backWallZ2TopFaces <- buildCubePointsListWithAdd "y1Z2Layer"
               (
                 (map ((transposeZ (+(z2Height))) . extractTopFace) btmBackWallRow)
               )
               backWallBackSealCutters
               

  backWallZ2Row <- buildCubePointsListWithAdd "backWallZ2Row"
               backWallZ2TopFaces
               (btmBackWallRow)
  
  backSealZ2TopFaces <- buildCubePointsListWithAdd "backSealZ2TopFaces"
               (
                 (map ((transposeZ (+(z2Height))) . extractTopFace) btmBackSealRow)
               )
               backWallBackSealCutters

  backSealZ2Row <- buildCubePointsListWithAdd "backSealZ2Row"
               backSealZ2TopFaces
               (btmBackSealRow)
  
  boardZ2TopFaces
           <- buildCubePointsListWithAdd "y4Z2Layer"
               (
                 (map ((transposeZ (+(z2Height))) . extractTopFace) btmBoardRow)
               )
               ([CornerPointsId, CornerPointsId, --left wall/seal
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsNothing, --center
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsId, CornerPointsId --right wall/seal
                 ])
  
  boardZ2Row <- buildCubePointsListWithAdd "y1Z2Layer"
               btmBoardRow
               boardZ2TopFaces
               
  
  motorZ2TopFaces
           <- buildCubePointsListWithAdd "y4Z2Layer"
               (
                 (map ((transposeZ (+(z2Height))) . extractTopFace) btmMotorRow)
               )
               ([CornerPointsId, CornerPointsId, --left wall/seal
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsId, --center
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsId, CornerPointsId --right wall/seal
                 ])

  motorZ2Row
        <- buildCubePointsListWithAdd "y5Z2Layer"
           btmMotorRow
           motorZ2TopFaces

  let frontSealFrontWallCutters =
                [CornerPointsId, CornerPointsId, --left wall/seal
                 CornerPointsId, CornerPointsId, CornerPointsId, CornerPointsId, CornerPointsId,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsId, CornerPointsId,
                 CornerPointsId, --center
                 CornerPointsId, CornerPointsId, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsId, CornerPointsId, CornerPointsId, CornerPointsId, CornerPointsId,
                 CornerPointsId, CornerPointsId --right wall/seal
                 ]
                
  frontSealZ2TopFaces
           <- buildCubePointsListWithAdd "y5Z2Layer"
               (
                 (map ((transposeZ (+(z2Height))) . extractTopFace) btmFrontSealRow)
               )
               (frontSealFrontWallCutters)

  frontSealZ2Row
        <- buildCubePointsListWithAdd "y5Z2Layer"
           btmFrontSealRow
           frontSealZ2TopFaces

  frontWallZ2TopFaces
           <- buildCubePointsListWithAdd "y6Z2Layer"
               (
                 (map ((transposeZ (+(z2Height))) . extractTopFace) btmFrontWallRow)
               )
               (frontSealFrontWallCutters)

  frontWallZ2Row
        <- buildCubePointsListWithAdd "y6Z2Layer"
           btmFrontWallRow
           frontWallZ2TopFaces

  -- =================================================== layer 3:========================================
  -- the seal layer on which the lid sits
  backWallZ3Row <- buildCubePointsListWithAdd "y1Z3Layer"
               (
                 (map ((transposeZ (+(z3Height))) . extractTopFace) backWallZ2Row)
               )
               backWallZ2Row
               

  backSealZ3Row <- buildCubePointsListWithAdd "backSealZ2TopFaces"
               (
                 (map ((transposeZ (+(z3Height))) . extractTopFace) backSealZ2Row)
               )
               backSealZ2Row

  boardZ3Row
           <- buildCubePointsListWithAdd "y4Z2Layer"
               (
                 (map ((transposeZ (+(z3Height))) . extractTopFace) boardZ2Row)
               )
               (boardZ2Row)
  
  motorZ3Row
           <- buildCubePointsListWithAdd "y4Z2Layer"
               (
                 (map ((transposeZ (+(z3Height))) . extractTopFace) motorZ2Row)
                 |+++|
                 motorZ2Row
               )
               ([CornerPointsId, CornerPointsId, --left wall/seal
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsId, CornerPointsId,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsId, CornerPointsId,
                 CornerPointsNothing, --center
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsId, CornerPointsId,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsId, CornerPointsId,
                 CornerPointsId, CornerPointsId --right wall/seal
                 ]
               )

  
  
                
  frontSealZ3Row
           <- buildCubePointsListWithAdd "y5Z2Layer"
               (
                 (map ((transposeZ (+(z3Height))) . extractTopFace) frontSealZ2Row)
               )
               (frontSealZ2Row)

  
  
  frontWallZ3Row
           <- buildCubePointsListWithAdd "y6Z2Layer"
               (
                 (map ((transposeZ (+(z3Height))) . extractTopFace) frontWallZ2Row)
               )
               (frontWallZ2Row)

  -- ===================== layer 4=================================================================================
  -- top of outer wall
  backWallZ4Row <- buildCubePointsListWithAdd "y1Z3Layer"
               (
                 (map ((transposeZ (+(z4Height))) . extractTopFace) backWallZ3Row)
               )
               backWallZ3Row
               
  let layer4Cutter =
               [ CornerPointsId, CornerPointsNothing, --left wall/seal
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsNothing, --center
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsNothing, CornerPointsId --right wall/seal
               ] 
  backSealZ4Row <- buildCubePointsListWithAdd "backSealZ2TopFaces"
               (
                 (map ((transposeZ (+(z4Height))) . extractTopFace) backSealZ3Row)
                 |+++|
                 backSealZ3Row
               )
               layer4Cutter

  boardZ4Row
           <- buildCubePointsListWithAdd "y4Z2Layer"
               (
                 (map ((transposeZ (+(z4Height))) . extractTopFace) boardZ3Row)
                 |+++|
                 boardZ3Row
               )
               layer4Cutter
  
  motorZ4Row
           <- buildCubePointsListWithAdd "y4Z2Layer"
               (
                 (map ((transposeZ (+(z4Height))) . extractTopFace) motorZ3Row)
                 |+++|
                 motorZ3Row
               )
               layer4Cutter
               

  
  
               
  frontSealZ4Row
           <- buildCubePointsListWithAdd "y5Z2Layer"
               (
                 (map ((transposeZ (+(z4Height))) . extractTopFace) frontSealZ3Row)
                 |+++|
                 frontSealZ3Row
               )
               (layer4Cutter)

  
  
  frontWallZ4Row
           <- buildCubePointsListWithAdd "y6Z2Layer"
               (
                 (map ((transposeZ (+(z4Height))) . extractTopFace) frontWallZ3Row)
               )
               (frontWallZ3Row)

  
  
  
  return backWallx10Cubes
{-before pattern matching on MotorMount
motorMountFromDb :: MotorMount ->  ExceptT BuilderError (State CpointsStack ) CpointsList
--motorMountFromDb (MotorMount name desc x1Width x2Width x3Width x4Width x5Width ) = do
motorMountFromDb motorMount = do
  
  y1x1BottomFaces
         <- buildCubePointsListSingle "y1x1BottomFaces"
            
             [
               ((B1 (Point 0 0 0)) +++ (B4 (Point (motorMount^.motorMountX1Width) 0 0)))
               +++
               --((F1 (Point 0 y1Width 0)) +++ (F4 (Point x1Width y1Width 0)))
               ((F1 (Point 0 (motorMount^.motorMountY1Width) 0)) +++ (F4 (Point (motorMount^.motorMountX1Width) (motorMount^.motorMountY1Width) 0)))
             ]
  y1x1Cubes
      <- buildCubePointsListWithAdd "y1x1Cubes"
         (map ((transposeZ (+ (motorMount^.motorMountZ1Height))) . upperFaceFromLowerFace) y1x1BottomFaces)
         y1x1BottomFaces

  y1x2Cubes
     <- buildCubePointsListWithAdd "y1x2Cubes"
        (map ((transposeX (+(motorMount^.motorMountX2Width))) . extractRightFace) y1x1Cubes)
        (y1x1Cubes)

  y1x3Cubes
     <- buildCubePointsListWithAdd "y1x3Cubes"
        (map ((transposeX (+(motorMount^.motorMountX3Width))) . extractRightFace) y1x2Cubes)
        (y1x2Cubes)

  y1x4Cubes
    <- buildCubePointsListWithAdd "y1x3Cubes"
       (map ((transposeX (+(motorMount^.motorMountX4Width))) . extractRightFace) y1x3Cubes)
        (y1x3Cubes)

  y1x5Cubes
    <- buildCubePointsListWithAdd "y1x3Cubes"
       (map ((transposeX (+(motorMount^.motorMountX5Width))) . extractRightFace) y1x4Cubes)
        (y1x4Cubes)

  y1x6Cubes
    <- buildCubePointsListWithAdd "y1x6Cubes"
       (map ((transposeX (+(motorMount^.motorMountX6Width))) . extractRightFace) y1x5Cubes)
        (y1x5Cubes)

  y1x7Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(motorMount^.motorMountX7Width))) . extractRightFace) y1x6Cubes)
        (y1x6Cubes)

  y1x8Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(motorMount^.motorMountX8Width))) . extractRightFace) y1x7Cubes)
        (y1x7Cubes)

  y1x9Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(motorMount^.motorMountX9Width))) . extractRightFace) y1x8Cubes)
        (y1x8Cubes)

  y1x10Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(motorMount^.motorMountX10Width))) . extractRightFace) y1x9Cubes)
        (y1x9Cubes)

  y1x11Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(motorMount^.motorMountX11Width))) . extractRightFace) y1x10Cubes)
        (y1x10Cubes)

  y1x12Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(motorMount^.motorMountX12Width))) . extractRightFace) y1x11Cubes)
        (y1x11Cubes)

  y1x13Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+(motorMount^.motorMountX13Width))) . extractRightFace) y1x12Cubes)
        (y1x12Cubes)

  let y1Row = y1x1Cubes  ++ y1x2Cubes ++ y1x3Cubes ++ y1x4Cubes ++ y1x5Cubes  ++y1x6Cubes ++ y1x7Cubes ++ y1x8Cubes ++ y1x9Cubes ++ y1x10Cubes ++ y1x11Cubes ++ y1x12Cubes ++ y1x13Cubes
  --y1 layer 1 done.
  --add rest of layer 1 y's by transposing along y axis
  y2Layer <- buildCubePointsListWithAdd "y2Layer"
             (y1Row)
             (map ((transposeY (+(motorMount^.motorMountY2Width))) . extractFrontFace) y1Row)

  y3Layer <- buildCubePointsListWithAdd "y2Layer"
             (y2Layer)
             (map ((transposeY (+(motorMount^.motorMountY3Width))) . extractFrontFace) y2Layer)

  y4Layer <- buildCubePointsListWithAdd "y2Layer"
             (y3Layer)
             (map ((transposeY (+(motorMount^.motorMountY4Width))) . extractFrontFace) y3Layer)
  
  y5Layer <- buildCubePointsListWithAdd "y2Layer"
             (y4Layer)
             (map ((transposeY (+(motorMount^.motorMountY5Width))) . extractFrontFace) y4Layer)

             --borderWidth
  y6Layer <- buildCubePointsListWithAdd "y2Layer"
             (y5Layer)
             (map ((transposeY (+(motorMount^.motorMountY6Width))) . extractFrontFace) y5Layer)


  -- ========== end of z layer 1(bottom layer)
  -- ========== start of layer 2
  let y1y2z2CutterCubes =
                [CornerPointsId, CornerPointsId, CornerPointsNothing,
                 CornerPointsId, CornerPointsId, CornerPointsNothing,
                 CornerPointsNothing,--center
                 CornerPointsId, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsId, CornerPointsId, CornerPointsId
                ]
  y1Z2TopFaces <- buildCubePointsListWithAdd "y1Z2Layer"
               (
                 (map ((transposeZ (+(motorMount^.motorMountZ2Height))) . extractTopFace) y1Row)
               )
               y1y2z2CutterCubes
               

  y1Z2Layer <- buildCubePointsListWithAdd "y1Z2Layer"
               y1Z2TopFaces
               (y1Row)
  
  y2Z2TopFaces <- buildCubePointsListWithAdd "y1Z2Layer"
               (
                 (map ((transposeZ (+(motorMount^.motorMountZ2Height))) . extractTopFace) y2Layer)
               )
               y1y2z2CutterCubesn

  y2Z2Layer <- buildCubePointsListWithAdd "y1Z2Layer"
               y2Z2TopFaces
               (y2Layer)
  
  y3Z2TopFaces
           <- buildCubePointsListWithAdd "y4Z2Layer"
               (
                 (map ((transposeZ (+(motorMount^.motorMountZ2Height))) . extractTopFace) y3Layer)
               )
               ([CornerPointsId, CornerPointsId, CornerPointsNothing,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsNothing,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsNothing, CornerPointsId, CornerPointsId
                 ])
  
  y3Z2Layer <- buildCubePointsListWithAdd "y1Z2Layer"
               y3Layer
               y3Z2TopFaces
  
  y4Z2TopFaces
           <- buildCubePointsListWithAdd "y4Z2Layer"
               (
                 (map ((transposeZ (+(motorMount^.motorMountZ2Height))) . extractTopFace) y4Layer)
               )
               ([CornerPointsId, CornerPointsId, CornerPointsNothing,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsId,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsNothing, CornerPointsId, CornerPointsId
                 ])

  y4Z2Layer
        <- buildCubePointsListWithAdd "y5Z2Layer"
           y4Layer
           y4Z2TopFaces

  let y5y6z2EndPlateIds =
                [CornerPointsId, CornerPointsId, CornerPointsId, CornerPointsId,
                 CornerPointsNothing, 
                 CornerPointsId, CornerPointsId, CornerPointsId,
                 CornerPointsNothing, 
                 CornerPointsId, CornerPointsId, CornerPointsId, CornerPointsId
                 ]
                
  y5Z2TopFaces
           <- buildCubePointsListWithAdd "y5Z2Layer"
               (
                 (map ((transposeZ (+(motorMount^.motorMountZ2Height))) . extractTopFace) y5Layer)
               )
               (y5y6z2EndPlateIds)

  y5Z2Layer
        <- buildCubePointsListWithAdd "y5Z2Layer"
           y5Layer
           y5Z2TopFaces

  y6Z2TopFaces
           <- buildCubePointsListWithAdd "y6Z2Layer"
               (
                 (map ((transposeZ (+(motorMount^.motorMountZ2Height))) . extractTopFace) y6Layer)
               )
               (y5y6z2EndPlateIds)

  y6Z2Layer
        <- buildCubePointsListWithAdd "y6Z2Layer"
           y6Layer
           y6Z2TopFaces

  -- ============== layer 3:
  -- the layer on which the lid sits
  
  
  return y1x13Cubes


-}
-- ============================ hard coded values motor mount============================================================
-- ======================================================================================================================
-- ======================================================================================================================
motorMountHardCoded :: ExceptT BuilderError (State CpointsStack ) CpointsList
motorMountHardCoded = do
  let 
      stdXWidth = 5.25
      borderWidth = 4
      sealWidth = 2
      
      x1Width = borderWidth
      x2Width = sealWidth
      x3Width = stdXWidth
      x4Width = stdXWidth
      x5Width = stdXWidth
      x6Width = stdXWidth
      x7Width = stdXWidth
      x8Width = stdXWidth
      x9Width = stdXWidth
      x10Width = stdXWidth
      x11Width = stdXWidth
      x12Width = sealWidth
      x13Width = borderWidth
      
      y1Width = borderWidth
      y3Width = 25
      y4Width = 40
      y5Width = sealWidth
      y6Width = borderWidth
      
      z1Height = 1  --btm plate thickness
      z2Height = 22 --motor height
      z3Height = 15 --board height
      z4Height = 2  --top plate thickness
      
  y1x1BottomFaces
         <- buildCubePointsListSingle "y1x1BottomFaces"
            
             [
               ((B1 (Point 0 0 0)) +++ (B4 (Point x1Width 0 0)))
               +++
               ((F1 (Point 0 y1Width 0)) +++ (F4 (Point x1Width y1Width 0)))
             ]
  
  y1x1Cubes
      <- buildCubePointsListWithAdd "y1x1Cubes"
         (map ((transposeZ (+z1Height)) . upperFaceFromLowerFace) y1x1BottomFaces)
         y1x1BottomFaces

  y1x2Cubes
     <- buildCubePointsListWithAdd "y1x2Cubes"
        (map ((transposeX (+x2Width)) . extractRightFace) y1x1Cubes)
        (y1x1Cubes)

  y1x3Cubes
     <- buildCubePointsListWithAdd "y1x3Cubes"
        (map ((transposeX (+x3Width)) . extractRightFace) y1x2Cubes)
        (y1x2Cubes)

  y1x4Cubes
    <- buildCubePointsListWithAdd "y1x3Cubes"
       (map ((transposeX (+x4Width)) . extractRightFace) y1x3Cubes)
        (y1x3Cubes)

  y1x5Cubes
    <- buildCubePointsListWithAdd "y1x3Cubes"
       (map ((transposeX (+x5Width)) . extractRightFace) y1x4Cubes)
        (y1x4Cubes)

  y1x6Cubes
    <- buildCubePointsListWithAdd "y1x6Cubes"
       (map ((transposeX (+x6Width)) . extractRightFace) y1x5Cubes)
        (y1x5Cubes)

  y1x7Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+x7Width)) . extractRightFace) y1x6Cubes)
        (y1x6Cubes)

  y1x8Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+x8Width)) . extractRightFace) y1x7Cubes)
        (y1x7Cubes)

  y1x9Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+x9Width)) . extractRightFace) y1x8Cubes)
        (y1x8Cubes)

  y1x10Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+x10Width)) . extractRightFace) y1x9Cubes)
        (y1x9Cubes)

  y1x11Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+x11Width)) . extractRightFace) y1x10Cubes)
        (y1x10Cubes)

  y1x12Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+x12Width)) . extractRightFace) y1x11Cubes)
        (y1x11Cubes)

  y1x13Cubes
    <- buildCubePointsListWithAdd "y1x7Cubes"
       (map ((transposeX (+x13Width)) . extractRightFace) y1x12Cubes)
        (y1x12Cubes)

  let y1RowOfCubes = y1x1Cubes  ++ y1x2Cubes ++ y1x3Cubes ++ y1x4Cubes ++ y1x5Cubes  ++y1x6Cubes ++ y1x7Cubes ++ y1x8Cubes ++ y1x9Cubes ++ y1x10Cubes ++ y1x11Cubes ++ y1x12Cubes ++ y1x13Cubes

  y2Layer <- buildCubePointsListWithAdd "y2Layer"
             (y1RowOfCubes)
             (map ((transposeY (+sealWidth)) . extractFrontFace) y1RowOfCubes)

  y3Layer <- buildCubePointsListWithAdd "y2Layer"
             (y2Layer)
             (map ((transposeY (+y3Width)) . extractFrontFace) y2Layer)

  y4Layer <- buildCubePointsListWithAdd "y2Layer"
             (y3Layer)
             (map ((transposeY (+y4Width)) . extractFrontFace) y3Layer)
  
  y5Layer <- buildCubePointsListWithAdd "y2Layer"
             (y4Layer)
             (map ((transposeY (+y5Width)) . extractFrontFace) y4Layer)

             --borderWidth
  y6Layer <- buildCubePointsListWithAdd "y2Layer"
             (y5Layer)
             (map ((transposeY (+y6Width)) . extractFrontFace) y5Layer)
  
  y1Z2Layer <- buildCubePointsListWithAdd "y1Z2Layer"
               (y1RowOfCubes)
               (
                 (map ((transposeZ (+z2Height)) . extractTopFace) y1RowOfCubes)
            )

  y2Z2Layer <- buildCubePointsListWithAdd "y1Z2Layer"
               (y2Layer)
               (
                 (map ((transposeZ (+z2Height)) . extractTopFace) y2Layer)
               )
  

  y3Z2Layer <- buildCubePointsListWithAdd "y1Z2Layer"
               (y3Layer)
               (
                 (map ((transposeZ (+z2Height)) . extractTopFace) y3Layer)
               )

  y4Z2TopFaces
           <- buildCubePointsListWithAdd "y4Z2Layer"
               (
                 (map ((transposeZ (+z2Height)) . extractTopFace) y4Layer)
               )
               ([CornerPointsId, CornerPointsId, CornerPointsId,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsId,
                 CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
                 CornerPointsId, CornerPointsId, CornerPointsId
                 ])

  y4Z2Layer
        <- buildCubePointsListWithAdd "y5Z2Layer"
           y4Layer
           y4Z2TopFaces

  let y5y6z2EndPlateIds =
                [CornerPointsId, CornerPointsId, CornerPointsId, CornerPointsId,
                 CornerPointsNothing, 
                 CornerPointsId, CornerPointsId, CornerPointsId,
                 CornerPointsNothing, 
                 CornerPointsId, CornerPointsId, CornerPointsId, CornerPointsId
                 ]
                
  y5Z2TopFaces
           <- buildCubePointsListWithAdd "y5Z2Layer"
               (
                 (map ((transposeZ (+z2Height)) . extractTopFace) y5Layer)
               )
               (y5y6z2EndPlateIds)

  y5Z2Layer
        <- buildCubePointsListWithAdd "y5Z2Layer"
           y5Layer
           y5Z2TopFaces

  y6Z2TopFaces
           <- buildCubePointsListWithAdd "y6Z2Layer"
               (
                 (map ((transposeZ (+z2Height)) . extractTopFace) y6Layer)
               )
               (y5y6z2EndPlateIds)

  y6Z2Layer
        <- buildCubePointsListWithAdd "y6Z2Layer"
           y6Layer
           y6Z2TopFaces

  y1Z3TopFaces
          <- buildCubePointsListWithAdd "y1Z3TopFaces"
             (map ((transposeZ (+(z3Height))) . extractTopFace) y1Z2Layer)
             (
             [CornerPointsId, CornerPointsId,
              CornerPointsNothing, CornerPointsNothing,
              CornerPointsId, CornerPointsId,
              CornerPointsNothing,
              CornerPointsId,
              CornerPointsNothing, CornerPointsNothing,
              CornerPointsId, CornerPointsId, CornerPointsId
             ])

  y1Z3Layer
           <- buildCubePointsListWithAdd "y1Z3Layer"
              y1Z3TopFaces
              y1Z2Layer
  


  y2Z3TopFaces
          <- buildCubePointsListWithAdd "y2Z3TopFaces"
             (map ((transposeZ (+(z3Height))) . extractTopFace) y2Z2Layer)
             (
             [CornerPointsId, CornerPointsId,
              CornerPointsNothing, CornerPointsNothing,
              CornerPointsId, CornerPointsId,
              CornerPointsNothing,
              CornerPointsId,
              CornerPointsNothing, CornerPointsNothing,
              CornerPointsId, CornerPointsId, CornerPointsId
             ])

  y2Z3Layer
           <- buildCubePointsListWithAdd "y2Z3Layer"
              y2Z3TopFaces
              y2Z2Layer
  
  y3Z3TopFaces
           <- buildCubePointsListWithAdd "y3Z3TopFaces"
              (map ((transposeZ (+(z3Height))) . extractTopFace) y3Z2Layer)
              [CornerPointsId, CornerPointsId,
               CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
               CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
               CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
               CornerPointsId, CornerPointsId
              ]

  y3Z3Layer
          <- buildCubePointsListWithAdd "y3Z3Layer"
             y3Z3TopFaces
             y3Z2Layer

  y4Z3TopFaces
           <- buildCubePointsListWithAdd "y4Z3TopFaces"
              (map ((transposeZ (+(z3Height))) . extractTopFace) y4Z2Layer)
              [CornerPointsId, CornerPointsId,
               CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
               CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
               CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
               CornerPointsId, CornerPointsId
              ]

  y4Z3Layer
          <- buildCubePointsListWithAdd "y4Z3Layer"
             y4Z3TopFaces
             y4Z2Layer

  y5Z3Layer
         <- buildCubePointsListWithAdd "y5Z3Layer"
            (map ((transposeZ (+(z3Height))) . extractTopFace) y5Z2Layer)
            y5Z2Layer
{-
  y6Z3And4Layer
         <- buildCubePointsListWithAdd "y6Z3And4Layer"
            (map ((transposeZ (+(z3Height+z4Height))) . extractTopFace) y6Z2Layer)
            y6Z2Layer
-}
  y6Z3Layer
         <- buildCubePointsListWithAdd "y6Z3Layer"
            (map ((transposeZ (+(z3Height))) . extractTopFace) y6Z2Layer)
            y6Z2Layer


  y1Z4TopFaces
          <- buildCubePointsListWithAdd "y1Z4TopFaces"
             (map ((transposeZ (+(z4Height))) . extractTopFace) y1Z3Layer)
             (
             [CornerPointsId, CornerPointsId,
              CornerPointsNothing, CornerPointsNothing,
              CornerPointsId, CornerPointsId,
              CornerPointsNothing,
              CornerPointsId,
              CornerPointsNothing, CornerPointsNothing,
              CornerPointsId, CornerPointsId, CornerPointsId
             ])
  
  y1Z4Layer
           <- buildCubePointsListWithAdd "y1Z4Layer"
              y1Z4TopFaces
              y1Z3Layer
  
  y2Z4TopFaces
        <- buildCubePointsListWithAdd "y2Z4TopFaces"
           (map ((transposeZ (+(z4Height))) . extractTopFace) y2Z3Layer)
           [CornerPointsId,
            CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
            CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
            CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
            CornerPointsNothing, CornerPointsNothing,
            CornerPointsId
           ]

  y2Z4Layer
       <- buildCubePointsListWithAdd "y2Z4Layer"
          y2Z4TopFaces
          y2Z3Layer

  y3Z4TopFaces
        <- buildCubePointsListWithAdd "y3Z4TopFaces"
           (map ((transposeZ (+(z4Height))) . extractTopFace) y3Z3Layer)
           [CornerPointsId,
            CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
            CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
            CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
            CornerPointsNothing, CornerPointsNothing,
            CornerPointsId
           ]

  y3Z4Layer
       <- buildCubePointsListWithAdd "y3Z4Layer"
          y3Z4TopFaces
          y3Z3Layer

  y4Z4TopFaces
        <- buildCubePointsListWithAdd "y4Z4TopFaces"
           (map ((transposeZ (+(z4Height))) . extractTopFace) y4Z3Layer)
           [CornerPointsId,
            CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
            CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
            CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
            CornerPointsNothing, CornerPointsNothing,
            CornerPointsId
           ]

  y4Z4Layer
       <- buildCubePointsListWithAdd "y4Z4Layer"
          y4Z4TopFaces
          y4Z3Layer

  y5Z4TopFaces
        <- buildCubePointsListWithAdd "y5Z4TopFaces"
           (map ((transposeZ (+(z4Height))) . extractTopFace) y5Z3Layer)
           [CornerPointsId,
            CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
            CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
            CornerPointsNothing, CornerPointsNothing, CornerPointsNothing,
            CornerPointsNothing, CornerPointsNothing,
            CornerPointsId
           ]

  y5Z4Layer
       <- buildCubePointsListWithAdd "y5Z4Layer"
          y5Z4TopFaces
          y5Z3Layer

  y6Z4Layer
         <- buildCubePointsListWithAdd "y6Z4Layer"
            (map ((transposeZ (+(z4Height))) . extractTopFace) y6Z3Layer)
            y6Z3Layer
            
  return y5Z4Layer


motorMountHardCodedStlGenerator :: IO ()
motorMountHardCodedStlGenerator = do
  let cpoints =  ((execState $ runExceptT   motorMountHardCoded       ) [])
  writeStlToFile $ newStlShape "motorMountHardCoded"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)


motorMountHardCodedShowCubes :: IO ()
motorMountHardCodedShowCubes = do
  let cpoints =  ((evalState $ runExceptT   motorMountHardCoded       ) [])
  print $ show cpoints
  
