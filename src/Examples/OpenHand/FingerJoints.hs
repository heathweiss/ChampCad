module Examples.OpenHand.FingerJoints(fingerJointsStlGenerator, fingerJointsToRiserStlGenerator) where

{- |
Build the flexible joints between the fingers, and between the finger and the riser.
fingerJoints: joins the finger sections together.

fingerToRiser: joins a finger joint to the riser.
Has the larger cylinder to attach to the finger joint.
Has a long flat section to attach to the riser.
-}

import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|), (+++>))
import CornerPoints.HorizontalFaces(createTopFaces, createBottomFaces)
import CornerPoints.Points(Point(..))
import CornerPoints.Transpose(transposeX, transposeZ)
import CornerPoints.Radius(Radius(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)
import CornerPoints.FaceExtraction(extractFrontFace)
import CornerPoints.FaceConversions(reverseNormal)

import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)

import Geometry.Angle(Angle(..))

import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsList,
                     CpointsStack, CpointsList, buildCubePointsListWithAdd, buildCubePointsListSingle)

import Control.Monad.Trans.Except
import Control.Monad.State.Lazy

-- 
fingerJoints :: ExceptT BuilderError (State CpointsStack ) CpointsList
fingerJoints = do
  let rightAngleStart = 50
      leftAngleStart = 230
      angleSpread = 80
      rightAngles = map Angle $ [0,10.. rightAngleStart] ++ [(rightAngleStart + angleSpread),(rightAngleStart + angleSpread + 10)..360]
      --to thin of joiner section
      --[0,10.. 70] ++ [110,120..360]
      leftAngles = map Angle $ [0,10.. leftAngleStart] ++ [(leftAngleStart + angleSpread) ,(leftAngleStart + angleSpread + 10)..360]
      --to thin
      --[0,10.. 250] ++ [290,300..360]
      withBetweenCenters  = 15
        --10 was not enough. Joints barely cleared resulting in pooer flex.
      btmRightOrigin = Point 0 0 0
      btmLeftOrigin = transposeX (+ withBetweenCenters) btmRightOrigin
      radii = repeat $ Radius 1.75
      height = 10
      
  rightCylinder
    <- buildCubePointsListSingle "right"
       ( 
         (createBottomFaces btmRightOrigin radii rightAngles )
         |+++|
         (createTopFaces (transposeZ (+ height) btmRightOrigin ) radii rightAngles )
       )

  leftCylinder
    <- buildCubePointsListSingle "left"
       ( 
         (createBottomFaces btmLeftOrigin radii leftAngles )
         |+++|
         (createTopFaces (transposeZ (+ height) btmLeftOrigin ) radii leftAngles )
       )

  joinerLeftBtm
    <- buildCubePointsListSingle "joiner left bottom"
       (createBottomFaces btmLeftOrigin radii [Angle leftAngleStart, Angle (leftAngleStart + angleSpread)])

  joinerLeftTop
    <- buildCubePointsListSingle "joiner left bottom"
       --(createTopFaces (transposeZ (+ height) btmLeftOrigin) radii [Angle 250, Angle 290])
       (createTopFaces (transposeZ (+ height) btmLeftOrigin) radii [Angle leftAngleStart, Angle (leftAngleStart + angleSpread)])

  {-
  Made up of full CubePoints so use a let statement so as not to push onto the state.
  joinerRight [FrontFace] is added to it to create the cubes for attaching the cylinders.
  -}
  let joinerLeft = joinerLeftBtm |+++| joinerLeftTop
  
  joinerRightBtm
    <- buildCubePointsListSingle "joiner rigth bottom"
       --( createBottomFaces btmRightOrigin radii [Angle 70, Angle 110])
       ( createBottomFaces btmRightOrigin radii [Angle rightAngleStart, Angle (rightAngleStart + angleSpread)])

  joinerRightTop
    <- buildCubePointsListSingle "joiner left bottom"
       --( createTopFaces (transposeZ (+ height) btmRightOrigin) radii [Angle 70, Angle 110])
       ( createTopFaces (transposeZ (+ height) btmRightOrigin) radii [Angle rightAngleStart, Angle (rightAngleStart + angleSpread)])

  joinerRight
    <- buildCubePointsListSingle "joiner left"
       ( map (reverseNormal . extractFrontFace)
         (joinerRightBtm |+++| joinerRightTop)
       )
       
  joiner
    <- buildCubePointsListWithAdd "joiner"
       joinerLeft
       joinerRight
       
  return rightCylinder


fingerJointsStlGenerator :: IO ()
fingerJointsStlGenerator = do
  let cpoints = ((execState $ runExceptT fingerJoints)  [])
  writeStlToFile $ newStlShape "finger joints"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)


-- ==================================================================================================================================================
{-
Join a finger joint to the riser.
Uses a cylinder for the finger joint, and a long flat section to join to the riser.
-}
fingerJointToRiser :: ExceptT BuilderError (State CpointsStack ) CpointsList
fingerJointToRiser = do
  let rightAngleStart = 50
      leftAngleStart = 230
      angleSpread = 80
      rightAngles = map Angle $ [0,10.. rightAngleStart] ++ [(rightAngleStart + angleSpread),(rightAngleStart + angleSpread + 10)..360]
      --to thin of joiner section
      --[0,10.. 70] ++ [110,120..360]
      leftAngles = map Angle $ [0,10.. leftAngleStart] ++ [(leftAngleStart + angleSpread) ,(leftAngleStart + angleSpread + 10)..360]
      --to thin
      --[0,10.. 250] ++ [290,300..360]
      withBetweenCenters  = 20
      btmRightOrigin = Point 0 0 0
      btmLeftOrigin = transposeX (+ withBetweenCenters) btmRightOrigin
      radii = repeat $ Radius 1.75
      height =10
      
  rightCylinder
    <- buildCubePointsListSingle "right"
       ( 
         (createBottomFaces btmRightOrigin radii rightAngles )
         |+++|
         (createTopFaces (transposeZ (+ height) btmRightOrigin ) radii rightAngles )
       )

  joinerLeftBtm
    <- buildCubePointsListSingle "joiner left bottom"
       (createBottomFaces btmLeftOrigin radii [Angle leftAngleStart, Angle (leftAngleStart + angleSpread)])

  joinerLeftTop
    <- buildCubePointsListSingle "joiner left bottom"
       --(createTopFaces (transposeZ (+ height) btmLeftOrigin) radii [Angle 250, Angle 290])
       (createTopFaces (transposeZ (+ height) btmLeftOrigin) radii [Angle leftAngleStart, Angle (leftAngleStart + angleSpread)])

  {-
  Made up of full CubePoints so use a let statement so as not to push onto the state.
  joinerRight [FrontFace] is added to it to create the cubes for attaching the cylinders.
  -}
  let joinerLeft = joinerLeftBtm |+++| joinerLeftTop
  
  joinerRightBtm
    <- buildCubePointsListSingle "joiner rigth bottom"
       --( createBottomFaces btmRightOrigin radii [Angle 70, Angle 110])
       ( createBottomFaces btmRightOrigin radii [Angle rightAngleStart, Angle (rightAngleStart + angleSpread)])

  joinerRightTop
    <- buildCubePointsListSingle "joiner left bottom"
       --( createTopFaces (transposeZ (+ height) btmRightOrigin) radii [Angle 70, Angle 110])
       ( createTopFaces (transposeZ (+ height) btmRightOrigin) radii [Angle rightAngleStart, Angle (rightAngleStart + angleSpread)])

  joinerRight
    <- buildCubePointsListSingle "joiner left"
       ( map (reverseNormal . extractFrontFace)
         (joinerRightBtm |+++| joinerRightTop)
       )
       
  joiner
    <- buildCubePointsListWithAdd "joiner"
       joinerLeft
       joinerRight
       
  return rightCylinder


fingerJointsToRiserStlGenerator :: IO ()
fingerJointsToRiserStlGenerator = do
  let cpoints = ((execState $ runExceptT fingerJointToRiser)  [])
  writeStlToFile $ newStlShape "finger joints"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
