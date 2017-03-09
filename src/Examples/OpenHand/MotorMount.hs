{- |
The container into which the motors and board will be mounted.
-}

module Examples.OpenHand.MotorMount(motorMountStlGenerator, motorMountShowCubes) where

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

motorMount :: ExceptT BuilderError (State CpointsStack ) CpointsList
motorMount = do
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

  let y1Layer = y1x1Cubes  ++ y1x2Cubes ++ y1x3Cubes ++ y1x4Cubes ++ y1x5Cubes  ++y1x6Cubes ++ y1x7Cubes ++ y1x8Cubes ++ y1x9Cubes ++ y1x10Cubes ++ y1x11Cubes ++ y1x12Cubes ++ y1x13Cubes

  y2Layer <- buildCubePointsListWithAdd "y2Layer"
             (y1Layer)
             (map ((transposeY (+sealWidth)) . extractFrontFace) y1Layer)

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
               (y1Layer)
               (
                 (map ((transposeZ (+z2Height)) . extractTopFace) y1Layer)
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


motorMountStlGenerator :: IO ()
motorMountStlGenerator = do
  let cpoints =  ((execState $ runExceptT   motorMount       ) [])
  writeStlToFile $ newStlShape "motorMount"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)


motorMountShowCubes :: IO ()
motorMountShowCubes = do
  let cpoints =  ((evalState $ runExceptT   motorMount       ) [])
  print $ show cpoints
  
