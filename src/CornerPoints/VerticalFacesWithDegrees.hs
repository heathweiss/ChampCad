{-# LANGUAGE ParallelListComp #-}
{- |
This module does not seem to be used anywhere. Should it be removed?
It seems to be a copy of CornerPoints.VerticalFaces.
-}
module CornerPoints.VerticalFacesWithDegrees where
import qualified  CornerPoints.VerticalFaces  as Vertical (
  createRightFaces, createRightFacesNoSlope, createVerticalFaces,
  createLeftFaces, createLeftFacesNoSlope,
  createHorizontallyAlignedCubes, createHorizontallyAlignedCubesNoSlope,
  createLeftFacesMultiColumns, createLeftFacesMultiColumnsNoSlope, {-createVerticalWalls,-}
  TransposeFactor(..))
import CornerPoints.Radius(SingleDegreeRadii(..), Radius(..), MultiDegreeRadii(..))
import CornerPoints.Create(Origin(..))
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..),(+++~>),(|+++~|), (+++~), DegreeRange(..))
import CornerPoints.CornerPoints(CornerPoints(..),)
import CornerPoints.Points (Point(..))
import CornerPoints.FaceExtractionWithDegrees (extractFrontFace)
import CornerPoints.FaceConversionsWithDegrees (backFaceFromFrontFace)

import Test.HUnit

import Geometry.Slope(Slope(..), flatXSlope, flatYSlope, slopeAdjustedForVerticalAngle)

--make type signatures more readable
type Degree = Double

createLeftFaces :: Degree -> Origin -> SingleDegreeRadii -> Slope -> Slope -> [Vertical.TransposeFactor] -> [CornerPointsWithDegrees]
createLeftFaces degrees origin singleDegreeRadii xSlope ySlope zTransposeFactor =
  let leftFaces = Vertical.createLeftFaces origin singleDegreeRadii xSlope ySlope zTransposeFactor
  in [LeftFaceWithDegrees face (DegreeRange degrees degrees) | face <- leftFaces]


createLeftFacesNoSlope :: Origin -> SingleDegreeRadii -> [Vertical.TransposeFactor] -> [CornerPointsWithDegrees]
createLeftFacesNoSlope origin singleDegreeRadii zTransposeFactor  =
  createLeftVerticalFaces origin singleDegreeRadii flatXSlope flatYSlope zTransposeFactor (F2) (B2) (F1) (B1)

createRightFaces :: Degree -> Origin -> SingleDegreeRadii -> Slope -> Slope -> [Vertical.TransposeFactor] -> [CornerPointsWithDegrees]
createRightFaces degrees origin singleDegreeRadii xSlope ySlope zTransposeFactor =
  let rightFaces = Vertical.createRightFaces origin singleDegreeRadii xSlope ySlope zTransposeFactor
  in [RightFaceWithDegrees face (DegreeRange degrees degrees) | face <- rightFaces]

createRightFacesNoSlope :: Origin -> SingleDegreeRadii -> [Vertical.TransposeFactor] -> [CornerPointsWithDegrees]
createRightFacesNoSlope origin singleDegreeRadii zTransposeFactor  =
  createRightVerticalFaces origin singleDegreeRadii flatXSlope flatYSlope zTransposeFactor (F3) (B3) (F4) (B4)

{-Same as createHorizontallyAlignedCubes, but with degree info attached. -}
createHorizontallyAlignedCubes :: [CornerPointsWithDegrees] -> [[CornerPointsWithDegrees]] -> [[CornerPointsWithDegrees]]
createHorizontallyAlignedCubes ([]) _ = []
createHorizontallyAlignedCubes (x:xs) (ys) =
  let headOfLeftFaces = map (head) ys
  in (x +++~> headOfLeftFaces) : (createHorizontallyAlignedCubes xs (map (tail) ys) )

createHorizontallyAlignedCubesNoSlope :: Origin -> MultiDegreeRadii -> [Vertical.TransposeFactor] -> [[CornerPointsWithDegrees]]
createHorizontallyAlignedCubesNoSlope origin (MultiDegreeRadii name' degrees') transposeFactors =
  let leftFaces = createLeftFacesMultiColumnsNoSlope origin (tail degrees') transposeFactors
      rightFaces = createRightFacesNoSlope origin (head degrees' )  transposeFactors
  in  createHorizontallyAlignedCubes rightFaces leftFaces



createLeftFacesMultiColumns ::  Origin -> [SingleDegreeRadii] -> Slope -> Slope -> [Vertical.TransposeFactor] -> [[CornerPointsWithDegrees]]
createLeftFacesMultiColumns _ [] _ _ _ = []
createLeftFacesMultiColumns topOrigin (d:ds) xSlope ySlope zTransposeFactor =
  (createLeftFaces (degree d) topOrigin d xSlope ySlope zTransposeFactor ) :
    (createLeftFacesMultiColumns topOrigin ds xSlope ySlope zTransposeFactor)



createLeftFacesMultiColumnsNoSlope ::  Origin -> [SingleDegreeRadii] -> [Vertical.TransposeFactor] -> [[CornerPointsWithDegrees]]
createLeftFacesMultiColumnsNoSlope _ [] _ = []
createLeftFacesMultiColumnsNoSlope topOrigin (d:ds) zTransposeFactor =
  (createLeftFacesNoSlope topOrigin d zTransposeFactor ) :
    (createLeftFacesMultiColumnsNoSlope topOrigin ds zTransposeFactor)


{-LeftFace version of createVerticalFaces-}
createLeftVerticalFaces :: Origin -> SingleDegreeRadii -> Slope -> Slope -> [Vertical.TransposeFactor] -> (Point-> CornerPoints) ->
                       (Point-> CornerPoints) -> (Point-> CornerPoints) -> (Point-> CornerPoints) -> [CornerPointsWithDegrees]
createLeftVerticalFaces origin (SingleDegreeRadii degree' radii') xSlope ySlope zTransposeFactor topFrontConstructor topBackConstructor
                            btmFrontConstructor btmBackConstructor =
  
  let verticalFaces = Vertical.createVerticalFaces origin (SingleDegreeRadii degree' radii') xSlope ySlope zTransposeFactor topFrontConstructor topBackConstructor
                            btmFrontConstructor btmBackConstructor
  --in  [LeftFaceWithDegrees  x degree' | x <- verticalFaces ]
  in  [LeftFaceWithDegrees  x (DegreeRange degree' degree') | x <- verticalFaces ]

{-RightFace version of createVerticalFaces-}
createRightVerticalFaces :: Origin -> SingleDegreeRadii -> Slope -> Slope -> [Vertical.TransposeFactor] -> (Point-> CornerPoints) ->
                       (Point-> CornerPoints) -> (Point-> CornerPoints) -> (Point-> CornerPoints) -> [CornerPointsWithDegrees]
createRightVerticalFaces origin (SingleDegreeRadii degree' radii') xSlope ySlope zTransposeFactor topFrontConstructor topBackConstructor
                            btmFrontConstructor btmBackConstructor =
  
  let verticalFaces = Vertical.createVerticalFaces origin (SingleDegreeRadii degree' radii') xSlope ySlope zTransposeFactor topFrontConstructor topBackConstructor
                            btmFrontConstructor btmBackConstructor
  in  [RightFaceWithDegrees  x (DegreeRange degree' degree') | x <- verticalFaces ]



{- |Create a vertical shape with walls based on an inner and outer MultiDegreeRadii-}
createVerticalWalls ::  MultiDegreeRadii ->   MultiDegreeRadii ->      Origin -> [Vertical.TransposeFactor] -> [[CornerPointsWithDegrees]]
createVerticalWalls     multiDegreeRadiiInner    multiDegreeRadiiOuter origin    transposeFactors =
       [
        currBackFace |+++~| currFrontFace
        | currFrontFace <- [map (extractFrontFace) currRow  | currRow  <- createHorizontallyAlignedCubesNoSlope origin multiDegreeRadiiOuter transposeFactors] 
        | currBackFace <- [ map (backFaceFromFrontFace . extractFrontFace) currRow | currRow  <- (createHorizontallyAlignedCubesNoSlope origin multiDegreeRadiiInner transposeFactors ) ]
       ]


testRightFace = 
           (RightFace (Point 1 0 1) (Point 1 0 0) (Point 1 1 1) (Point 1 1 0))

testLeftFace = 
           (LeftFace  (Point 0 0 0)  (Point 0 0 1)  (Point 0 1 0) (Point 0 1 1) )
           
verticalFacesWithDegreesTest = do
  {-Create a column of left faces that have degree info attached.-}
  let createLeftFacesColumnTest = TestCase $ assertEqual
        "createLeftFacesColumnTest"
        ( [LeftFaceWithDegrees
           {_leftFace = LeftFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
                              f2 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0}, f1 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}},
            _degreeRange = (DegreeRange 0.0 0.0)}, 
           LeftFaceWithDegrees
            {_leftFace = LeftFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0},
                              f2 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}, f1 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 8.0}},
            _degreeRange = (DegreeRange 0.0 0.0)} 
          ]

        )
        ( let sdr = (SingleDegreeRadii 0 (map (Radius) [1,2,2]))
          in  createLeftFaces (0::Degree) (Point 0 0 10) sdr flatXSlope flatYSlope [1,2..]
        )
  runTestTT createLeftFacesColumnTest

  {-Create a column of right faces that have degree info attached.-}
  let createRightFacesColumnTest = TestCase $ assertEqual
        "createRightFacesColumnTest"
        ( [RightFaceWithDegrees
           {_rightFace = RightFace {b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
                              f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0}, f4 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}},
             _degreeRange = (DegreeRange 0.0 0.0)}, 
           RightFaceWithDegrees
            {_rightFace = RightFace {b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0},
                              f3 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}, f4 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 8.0}},
            _degreeRange = (DegreeRange 0.0 0.0)} 
          ]

        )
        ( let sdr = (SingleDegreeRadii 0 (map (Radius) [1,2,2]))
          in  createRightFaces (0::Degree) (Point 0 0 10) sdr flatXSlope flatYSlope [1,2..]
        )
  runTestTT createRightFacesColumnTest


