{-# LANGUAGE ParallelListComp #-}
module Examples.Primitives.Cylinders(slopedToppedCylinder, solidCylinderSquared, solidCylinderLengthenY, walledCylinderSquared) where
import qualified Primitives.Cylindrical.Walled as Walled (cylinder, squaredCylinder, squaredYLengthenedCylinder) 
import  Primitives.Cylindrical.Solid(yLengthenedCylinder, squaredOffCylinder, squaredOffYLengthenedCylinder, slopedTopCylinder)
import CornerPoints.Radius(Radius(..))
import CornerPoints.Points(Point(..))
import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import CornerPoints.Create(Angle(..), Slope(..), flatXSlope, flatYSlope,)
import CornerPoints.Transpose(transposeY)

angles = (map (Angle) [0,10..360])

slopedToppedCylinder = 
  let cylinder = slopedTopCylinder [Radius r | r <- [10,10..]] (Point 0 0 0) angles [NegXSlope s | s <- [10,10..]] [NegYSlope s | s <- [10,10..]] (10 :: Height)
      cylinderTriangles =  [FacesBackBottomFrontTop | x <- [1..35]]
             |+++^|
             cylinder
      cylinderStl = newStlShape "cylinder" cylinderTriangles
  in  writeStlToFile cylinderStl
      
solidCylinderSquared =
  let cylinder = squaredOffCylinder (Radius 10) (Point 0 0 0) angles (10 :: Height) (5 :: Power) 
      cylinderTriangles =  [FacesBackBottomFrontTop | x <- [1..35]]
             |+++^|
             cylinder
      cylinderStl = newStlShape "cylinder" cylinderTriangles
  in  writeStlToFile cylinderStl

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
