{-# LANGUAGE ParallelListComp #-}
module Examples.Primitives.Cylinders where
import Primitives.Cylindrical({-cylinderWallsNoSlope,-} cylinderWallsNoSlopeSquaredOff,
                              cylinderWallsNoSlopeSquaredOffLengthenY)
import qualified Primitives.Cylindrical as Walled (cylinder) 
import  Primitives.Cylindrical.Solid(yLengthenedCylinder, squaredOffCylinder, squaredOffYLengthenedCylinder)
import CornerPoints.Radius(Radius(..))
import CornerPoints.Points(Point(..))
import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import CornerPoints.Create(Angle(..))
import CornerPoints.Transpose(transposeY)

angles = (map (Angle) [0,10..360])

solidCylinderSquared =
  let cylinder = squaredOffCylinder (Radius 10) (Point 0 0 0) angles (10 :: Height) (30 :: Power) 
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
  let --cylinder = cylinderWallsNoSlopeSquaredOff (Radius 100) (10::Thickness)  (Point 0 0 0) angles (10 :: Height)  (10 :: Power)
      cylinder = cylinderWallsNoSlopeSquaredOff [(Radius 100)| x <-[1..]] (10::Thickness)  (Point 0 0 0) angles (10 :: Height)  (10 :: Power)
      --cylinderWallsNoSlope ::                  Radius -> Thickness ->       Origin ->    [Angle] -> Height -> [CornerPoints]
      cylinderTriangles = FacesBackBottomFront :  [FacesBackBottomFrontTop | x <- [1..]]
             |+++^|
             cylinder
      cylinderStl = newStlShape "cylinder" cylinderTriangles
  in  writeStlToFile cylinderStl

        --cylinderWallsNoSlopeSquaredOffLengthenY :: Radius -> Origin -> [Angle] -> Height ->    Thickness ->   Power -> LengthenFactor -> [CornerPoints]
walledCylinderSquaredLengthenY =
  let cylinder = cylinderWallsNoSlopeSquaredOffLengthenY (Radius 10)  (Point 0 0 0) angles (10 :: Height) (10::Thickness) (10 :: Power) (10::LengthenFactor)
      --cylinderWallsNoSlope ::                  Radius -> Thickness ->       Origin ->    [Angle] -> Height -> [CornerPoints]
      cylinderTriangles =   [FacesBackBottomFrontTop | x <- [1..]]
             |+++^|
             cylinder
      cylinderStl = newStlShape "cylinder" cylinderTriangles
  in  writeStlToFile cylinderStl


walledCylinder =
  let --cylinder = Walled.cylinder [(Radius 100)|x <- [1..]] (10::Thickness) (Point 0 0 0) angles (10 :: Height)
      cylinder = Walled.cylinder [(Radius 100)|x <- [1..]] [(Radius 110)|x <- [1..]]   angles  (Point 0 0 0) (10 :: Height)
      cylinderTriangles =  [FacesBackBottomFrontTop | x <- [1..35]]
             |+++^|
             cylinder
      cylinderStl = newStlShape "cylinder" cylinderTriangles
  in  writeStlToFile cylinderStl



      
type Power = Double
type Height = Double
type Thickness = Double
type LengthenFactor = Double
