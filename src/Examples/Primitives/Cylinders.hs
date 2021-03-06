{-# LANGUAGE ParallelListComp #-}
module Examples.Primitives.Cylinders(slopedToppedCylinder, solidCylinderSquared, solidCylinderLengthenY, walledCylinderSquared) where

import qualified Primitives.Cylindrical.Walled as Walled (cylinder, squaredCylinder, squaredYLengthenedCylinder) 
import  Primitives.Cylindrical.Solid(yLengthenedCylinder, squaredOffCylinder, squaredOffYLengthenedCylinder, slopedTopCylinder)

import CornerPoints.Radius(Radius(..))
import CornerPoints.Points(Point(..))
import CornerPoints.Transpose(transposeY)
import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)

import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsList,
                     CpointsStack, CpointsList, buildCubePointsListWithAdd, buildCubePointsListSingle)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader


import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)


import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..))
import Geometry.Slope(Slope(..), flatXSlope, flatYSlope, slopeAdjustedForVerticalAngle)

angles = (map (Angle) [0,10..360])

slopedToppedCylinder = 
  let cylinder = slopedTopCylinder [Radius r | r <- [10,10..]] (Point 0 0 0) angles [NegXSlope s | s <- [10,10..]] [NegYSlope s | s <- [10,10..]] (10 :: Height)
      cylinderTriangles =  [FacesBackBottomFrontTop | x <- [1..35]]
             |+++^|
             cylinder
      cylinderStl = newStlShape "cylinder" cylinderTriangles
  in  writeStlToFile cylinderStl
      
solidCylinderSquaredShowCpoints :: [CornerPoints]
solidCylinderSquaredShowCpoints =
  let solidCylinderSquared' :: ExceptT BuilderError (State CpointsStack ) CpointsList
      solidCylinderSquared' = do
        cylinder <- buildCubePointsListSingle "cylinder"
              $ squaredOffCylinder (Radius 10) (Point 0 0 0) angles (10 :: Height) (5 :: Power)
        
        return cylinder
  in
  case   ((evalState $ runExceptT solidCylinderSquared') [])  of
    Right cpoints -> cpoints
    Left  (BuilderError err)       -> [CornerPointsError err]

solidCylinderSquared :: IO ()
solidCylinderSquared =
  let solidCylinderSquared' :: ExceptT BuilderError (State CpointsStack ) CpointsList
      solidCylinderSquared' = do
        cylinder <- buildCubePointsListSingle "cylinder"
              $ squaredOffCylinder (Radius 10) (Point 0 0 0) angles (10 :: Height) (5 :: Power)
        
        return cylinder
      
      cpoints = ((execState $ runExceptT solidCylinderSquared') []) 
  in
  writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

solidCylinderLengthenY =
  let cylinder = yLengthenedCylinder (Radius 10) (Point 0 0 0) angles (10 :: Height)  (10 :: LengthenFactor) 
      cylinderTriangles =  [FacesBackBottomFrontTop | x <- [1..36]]
             |+++^|
             cylinder
      cylinderStl = newStlShape "cylinder" cylinderTriangles
  in  writeStlToFile cylinderStl

solidCylinderSquaredOffLengthenY =
  let cylinder = squaredOffYLengthenedCylinder (Radius 10) (Point 0 0 0) angles (10 :: Height) (10 :: Power)  (10::LengthenFactor)  
      cylinderTriangles =  [FacesBackBottomFrontTop | x <- [1..36]]
             |+++^|
             cylinder
      cylinderStl = newStlShape "cylinder" cylinderTriangles
  in  writeStlToFile cylinderStl
walledCylinderSquared =
  let cylinder = Walled.squaredCylinder [(Radius 10)| x <-[1..]] (10::Thickness)  (Point 0 0 0) angles (10 :: Height)  (10 :: Power)
      cylinderTriangles = FacesBackBottomFront :  [FacesBackBottomFrontTop | x <- [1..]]
             |+++^|
             cylinder
      cylinderStl = newStlShape "cylinder" cylinderTriangles
  in  writeStlToFile cylinderStl

        
walledCylinderSquaredLengthenY =
  let cylinder = Walled.squaredYLengthenedCylinder (Radius 10)  (Point 0 0 0) angles (10 :: Height) (10::Thickness) (10 :: Power) (10::LengthenFactor)
      cylinderTriangles =   [FacesBackBottomFrontTop | x <- [1..]]
             |+++^|
             cylinder
      cylinderStl = newStlShape "cylinder" cylinderTriangles
  in  writeStlToFile cylinderStl


walledCylinder =
  let cylinder = Walled.cylinder [(Radius 100)|x <- [1..]] [(Radius 110)|x <- [1..]]   angles  (Point 0 0 0) (10 :: Height)
      cylinderTriangles =  [FacesBackBottomFrontTop | x <- [1..35]]
             |+++^|
             cylinder
      cylinderStl = newStlShape "cylinder" cylinderTriangles
  in  writeStlToFile cylinderStl



      
type Power = Double
type Height = Double
type Thickness = Double
type LengthenFactor = Double
