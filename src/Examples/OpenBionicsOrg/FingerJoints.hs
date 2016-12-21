module Examples.OpenBionicsOrg.FingerJoints() where

{- |
Recreate the finger joints that would normally be make out of silicone sheets, so that they can instead be printed from NinjaFlex.
-}


import Stl.StlCornerPoints((|+++^|))
import Stl.StlCornerPoints( Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import Primitives.Cubical( rectangularCubeNonRadial)

{-
middle finger
My specs to print out as a single piece
110 mm x 18 x 4

OpenBionics specs which would print out multiple pieces for each finger.
Joint1:
Silicone Sheet, 60A Durometer [(35x18x3)mm]
Joint2
Silicone Sheet, 60A Durometer [(39x18x4)mm]
-}

jointCubes = rectangularCubeNonRadial 4 18 110

jointTriangles =  [FacesAll | x <- [1..]] |+++^| [jointCubes]

jointStl = newStlShape "finger joint" jointTriangles

writeJointStl = writeStlToFile  jointStl