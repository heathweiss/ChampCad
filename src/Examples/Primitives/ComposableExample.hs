{-# LANGUAGE ParallelListComp #-}
module Examples.Primitives.ComposableExample(createCylinderComposableSlopedStlGenerator, createCylinderComposableSlopedCumulativeCornerPoints,
                                            createDoubleCylinderComposableStlGenerator, createDoubleCylinderComposableCumulativeCornerPoints,
                                            createDoubleCylinderSquaredStlGenerator, createDoubleCylinderSquaredCumulativeCornerPoints,
                                            createDoubleCylinderSquaredAndSlopedStlGenerator, createDoubleCylinderSquaredAndSlopedCumulativeCornerPoints ) where

import CornerPoints.Composable (createCornerPoint, Origin(..), Composable(..), composableDefault, createCornerPointComposable,
                               createBottomFacesComposable, createCornerPointComposableSloped,  createTopFacesComposable, createComposable, addSlope)
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces, createTopFacesSloped)
import CornerPoints.Radius(Radius(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|), (+++>))
import CornerPoints.Create(Angle(..), Slope(..), rotateAngle, RotateFactor(..))
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace, extractFrontTopLine, extractBackTopLine,
                                    extractBackBottomLine, extractBackTopLine, extractBottomFrontLine)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace,
                                    backBottomLineFromBottomFrontLine, frontTopLineFromBackTopLine, backTopLineFromFrontTopLine)
import CornerPoints.Transpose (transposeZ, transposeY, transposeX, )
import CornerPoints.Points(Point(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)

import TypeClasses.Transposable(transpose)

import Geometry.Radius(doubleCylinderZip, doubleCylinder, squaredOff)

import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)

import Geometry.Radius(doubleCylinderZip)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader
import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsList, buildCubePointsListWithAdd,
                     CpointsStack, CpointsList)

createDoubleCylinderComposable :: ExceptT BuilderError (State CpointsStack ) CpointsList
createDoubleCylinderComposable = do
  let originBtm = Point 0 0 0
      originTop = Point 0 0 10
      radii = map (Radius) [10,10..]
      angles = map (Angle) [0,10..360]
      headBtm = createCornerPointComposable (F4) originBtm (head radii) (head angles)
      tailBtm = zipWith
                (createCornerPointComposable (F1) originBtm)
                (tail radii)
                (tail angles)
      radiiTop = doubleCylinderZip radii angles
      headTop = createCornerPointComposable  (F3) originTop (head radiiTop) (head angles)
      --This is the version to make them sloped
      --headTop = createCornerPointComposableSloped (NegXSlope 10) (NegYSlope 10) $ createComposable  (F3(Point 0 0 0)) originTop (head radiiTop) (head angles)
      tailTop = --map to make it sloped
                --map (createCornerPointComposableSloped (NegXSlope 10) (NegYSlope 10))
                 (zipWith
                   --to make it sloped
                   --(createComposable (F2(Point 0 0 0)) originTop )
                   (createCornerPointComposable (F2) originTop )
                   (tail radiiTop)
                   (tail angles)
                 )

  btmFaces <- buildCubePointsListWithAdd "btmFaces"
              (createBottomFacesComposable (headBtm : tailBtm))
              [CornerPointsId | x <-[1..]]

              
  topFaces <- buildCubePointsListWithAdd "topFaces"
              (createTopFacesComposable (headTop : tailTop))
              [CornerPointsId | x <-[1..]]

  cylinder <- buildCubePointsListWithAdd "cylinder"
              btmFaces
              topFaces
              
  
  return cylinder 

createDoubleCylinderComposableStlGenerator :: IO ()
createDoubleCylinderComposableStlGenerator = do
  let cpoints = ((execState $ runExceptT (createDoubleCylinderComposable)) [])
  writeStlToFile $ newStlShape "cpoinst"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

createDoubleCylinderComposableCumulativeCornerPoints :: IO ()
createDoubleCylinderComposableCumulativeCornerPoints    =
  print $ show  ((evalState $ runExceptT (createDoubleCylinderComposable) ) [])


createDoubleCylinderSquared :: ExceptT BuilderError (State CpointsStack ) CpointsList
createDoubleCylinderSquared = do
  let originBtm = Point 0 0 0
      originTop = Point 0 0 10
      radii = map (Radius) [10,10..]
      angles = map (Angle) [0,10..360]
      headBtm = createCornerPoint (F4) originBtm (head radii) (head angles)
                
      tailBtm = zipWith
                (createCornerPoint (F1) originBtm)
                (tail radii)
                (tail angles)
                --rotateAngle rotated the double cylinder, by adjusting the angle. Worked excellent.
      radiiTop  = [doubleCylinder r (rotateAngle 10 a) | r <- radii | a <- angles ]
                  --apply squared of as well. Can be done in either order with == results.
                  --[squaredOff 4 (doubleCylinder r a) a   | r <- radii | a <- angles ]--map (radiiTop') angles
                  --[ doubleCylinder (squaredOff 4 r a) a   | r <- radii | a <- angles ]--map (radiiTop') angles
      headTop = createCornerPoint  (F3) originTop (head radiiTop) (head angles)
      tailTop =  (zipWith
                   (createCornerPoint (F2) originTop )
                   (tail radiiTop)
                   (tail angles)
                 )

  btmFaces <- buildCubePointsListWithAdd "btmFaces"
              (createBottomFaces originBtm radii angles)
              [CornerPointsId | x <-[1..]]

              
  topFaces <- buildCubePointsListWithAdd "topFaces"
              (createTopFaces originTop radiiTop angles)
              [CornerPointsId | x <-[1..]]

  cylinder <- buildCubePointsListWithAdd "cylinder"
              btmFaces
              topFaces
              
  
  return cylinder

createDoubleCylinderSquaredStlGenerator :: IO ()
createDoubleCylinderSquaredStlGenerator = do
  let cpoints = ((execState $ runExceptT (createDoubleCylinderSquared)) [])
  writeStlToFile $ newStlShape "cpoinst"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

createDoubleCylinderSquaredCumulativeCornerPoints :: IO ()
createDoubleCylinderSquaredCumulativeCornerPoints    =
  print $ show  ((evalState $ runExceptT (createDoubleCylinderSquared) ) [])



createDoubleCylinderSquaredAndSloped :: ExceptT BuilderError (State CpointsStack ) CpointsList
createDoubleCylinderSquaredAndSloped = do
  let originBtm = Point 0 0 0
      originTop = Point 0 0 10
      radii = map (Radius) [10,10..]
      angles = map (Angle) [0,10..360]
      headBtm = createCornerPoint (F4) originBtm (head radii) (head angles)
                
      tailBtm = zipWith
                (createCornerPoint (F1) originBtm)
                (tail radii)
                (tail angles)
                --rotateAngle rotated the double cylinder, by adjusting the angle. Worked excellent.
      radiiTop  = [doubleCylinder r (rotateAngle 10 a) | r <- radii | a <- angles ]
                  --apply squared of as well. Can be done in either order with == results.
                  --[squaredOff 4 (doubleCylinder r a) a   | r <- radii | a <- angles ]--map (radiiTop') angles
                  --[ doubleCylinder (squaredOff 4 r a) a   | r <- radii | a <- angles ]--map (radiiTop') angles
      headTop = createCornerPoint  (F3) originTop (head radiiTop) (head angles)
      tailTop =  (zipWith
                   (createCornerPoint (F2) originTop )
                   (tail radiiTop)
                   (tail angles)
                 )

  btmFaces <- buildCubePointsListWithAdd "btmFaces"
              (createBottomFaces originBtm radii angles)
              [CornerPointsId | x <-[1..]]
  
  topFaces <- buildCubePointsListWithAdd "topFaces"
              (createTopFacesSloped  originTop radiiTop angles (PosXSlope 10) (PosYSlope 10))
              [CornerPointsId | x <-[1..]]


       
  cylinder <- buildCubePointsListWithAdd "cylinder"
              btmFaces
              topFaces
              
  
  return cylinder

createDoubleCylinderSquaredAndSlopedStlGenerator :: IO ()
createDoubleCylinderSquaredAndSlopedStlGenerator = do
  let cpoints = ((execState $ runExceptT (createDoubleCylinderSquaredAndSloped)) [])
  writeStlToFile $ newStlShape "cpoinst"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

createDoubleCylinderSquaredAndSlopedCumulativeCornerPoints :: IO ()
createDoubleCylinderSquaredAndSlopedCumulativeCornerPoints    =
  print $ show  ((evalState $ runExceptT (createDoubleCylinderSquaredAndSloped) ) [])



  
createCylinderComposableSloped :: ExceptT BuilderError (State CpointsStack ) CpointsList
createCylinderComposableSloped = do
  
  let originBtm = Point 0 0 0
      originTop = Point 0 0 10
      radii = map (Radius) [10,10..]
      angles = map (Angle) [0,10..360]
      headBtm = createCornerPointComposable (F4) originBtm (head radii) (head angles)
      tailBtm = zipWith
                (createCornerPointComposable (F1) originBtm)
                (tail radii)
                (tail angles)
      headTop = createCornerPointComposableSloped (NegXSlope 10) (NegYSlope 10) $ createComposable  (F3 (Point 0 0 0)) originTop (head radii) (head angles)
      tailTop = map (createCornerPointComposableSloped (NegXSlope 10) (NegYSlope 10)) 
                 (zipWith
                   --(createCornerPointComposable (F2) originTop )
                  (createComposable (F2 (Point 0 0 0)) originTop )
                   (tail radii)
                   (tail angles)
                 )
      
  
  btmFaces <- buildCubePointsListWithAdd "btmFaces"
              (createBottomFacesComposable (headBtm : tailBtm))
              [CornerPointsId | x <-[1..]]
              
  topFaces <- buildCubePointsListWithAdd "topFaces"
              (createTopFacesComposable (headTop : tailTop))
              [CornerPointsId | x <-[1..]]

  cylinder <- buildCubePointsListWithAdd "cylinder"
              btmFaces
              topFaces
              
  
  return cylinder 

createCylinderComposableSlopedStlGenerator :: IO ()
createCylinderComposableSlopedStlGenerator = do
  let cpoints = ((execState $ runExceptT (createCylinderComposableSloped)) [])
  writeStlToFile $ newStlShape "cpoinst"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

createCylinderComposableSlopedCumulativeCornerPoints :: IO ()
createCylinderComposableSlopedCumulativeCornerPoints    =
  print $ show  ((evalState $ runExceptT (createCylinderComposableSloped) ) [])


type Height = Double
