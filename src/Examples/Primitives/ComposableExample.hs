module Examples.Primitives.ComposableExample(cylinderStlGenerator, createCylinderComposableStlGenerator, createCylinderComposableCumulativeCornerPoints) where

import CornerPoints.Composable (createCornerPoint, Origin(..), createBottomFaces, Composable(..), composableDefault, createCornerPointComposable,
                               createBottomFacesComposable, createCornerPointComposableSloped,  createTopFacesComposable, createComposable)
import CornerPoints.Radius(Radius(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|))
import CornerPoints.Create(Angle(..), Slope(..))
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace, extractFrontTopLine, extractBackTopLine,
                                    extractBackBottomLine, extractBackTopLine, extractBottomFrontLine)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace,
                                    backBottomLineFromBottomFrontLine, frontTopLineFromBackTopLine, backTopLineFromFrontTopLine)
import CornerPoints.Transpose (transposeZ, transposeY, transposeX, )
import TypeClasses.Transposable(transpose)
import CornerPoints.Points(Point(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)

import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)

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
{-
walledCylinder =
  let cylinder = Walled.cylinder [(Radius 100)|x <- [1..]] [(Radius 110)|x <- [1..]]   angles  (Point 0 0 0) (10 :: Height)
      cylinderTriangles =  [FacesBackBottomFrontTop | x <- [1..35]]
             |+++^|
             cylinder
      cylinderStl = newStlShape "cylinder" cylinderTriangles
  in  writeStlToFile cylinderStl
-}

cylinder :: [Radius] ->     [Radius] ->     [Angle] -> Origin -> Double -> [CornerPoints]
cylinder    innerWallRadii  outerWallRadii  angles     origin    height  =
   ((--bottom faces
      (map (backBottomLineFromBottomFrontLine . extractBottomFrontLine) (createBottomFaces origin innerWallRadii angles )) 
      |+++|
      (map (extractBottomFrontLine) (createBottomFaces origin outerWallRadii angles  )) 
    )
    |@+++#@|
    ((transposeZ (+ height)) . upperFaceFromLowerFace)
   )
{-
cylinderComposable ::  [CornerPoints]
cylinderComposable    =
  let originBtm = Point 0 0 0
      originTop = Point 0 0 10
      radii = map (Radius) [10,10..]
      angles = map (Angle) [0,10..360]
      headBtm = createCornerPointComposable (F4) originBtm (head radii) (head angles)
      tailBtm = zipWith
                (createCornerPointComposable (F1) originBtm)
                (tail radii)
                (tail angles)
      headTop = createCornerPointComposableSloped (NegSlope 10) (NegYSlope 10) $ createCornerPointComposable  (F3) originTop (head radii) (head angles)
      tailTop = map (createCornerPointComposableSloped (NegSlope 10) (NegYSlope 10)) 
                 (zipWith
                   (createCornerPointComposable (F2) originTop )
                   (tail radii)
                   (tail angles)
                 )
      
      btmFaces = createBottomFacesComposable (headBtm : tailBtm)
      --
      topFaces = createTopFacesComposable (headTop : tailTop)
      --topFacesTemp = createBottomFacesComposable (createCornerPointComposableSloped flatXSlope flatYSlope headBtm  : map (createCornerPointComposableSloped (NegSlope 10) (NegYSlope 10)) tailBtm)
      --topFaces = map (upperFaceFromLowerFace . (transposeZ (+10))) topFacesTemp
  in  
      btmFaces  |+++| topFaces

-}
createCylinderComposable :: ExceptT BuilderError (State CpointsStack ) CpointsList
createCylinderComposable = do
  {-cyl <- buildCubePointsListWithAdd  "cyl"
         cylinderComposable
         [CornerPointsId | x <-[1..]]
  -}
  let originBtm = Point 0 0 0
      originTop = Point 0 0 10
      radii = map (Radius) [10,10..]
      angles = map (Angle) [0,10..360]
      headBtm = createCornerPointComposable (F4) originBtm (head radii) (head angles)
      tailBtm = zipWith
                (createCornerPointComposable (F1) originBtm)
                (tail radii)
                (tail angles)
      --headTop = createCornerPointComposableSloped (NegXSlope 10) (NegYSlope 10) $ createCornerPointComposable  (F3) originTop (head radii) (head angles)
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

createCylinderComposableStlGenerator :: IO ()
createCylinderComposableStlGenerator = do
  let cpoints = ((execState $ runExceptT (createCylinderComposable)) [])
  writeStlToFile $ newStlShape "cpoinst"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

createCylinderComposableCumulativeCornerPoints :: IO ()
createCylinderComposableCumulativeCornerPoints    =
  print $ show  ((evalState $ runExceptT (createCylinderComposable) ) [])

createCylinder :: ExceptT BuilderError (State CpointsStack ) CpointsList
createCylinder = do
  cyl <- buildCubePointsListWithAdd  "cyl"
         (cylinder [Radius r | r <- [5,5..]] [Radius r | r <-[10,10..]] [Angle a | a <- [0,10..360]] (Point 0 0 0) 10)
         [CornerPointsId | x <-[1..]]

  return cyl

cylinderStlGenerator :: IO ()
cylinderStlGenerator = do
  let cpoints = ((execState $ runExceptT (createCylinder)) [])
  writeStlToFile $ newStlShape "cpoinst"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

type Height = Double
