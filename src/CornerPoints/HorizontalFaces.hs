{-# LANGUAGE ParallelListComp #-}
module CornerPoints.HorizontalFaces(
  createTopFaces,
  createTopFacesSloped,
  createBottomFaces,
  createBottomFacesSloped,
  createTopFacesWithVariableSlope,
  createBottomFacesWithVariableSlope,
  createBottomFacesSquaredOff,
  createBottomFacesSquaredOffLengthenY,
  createBottomFacesLengthenY,
  createBottomFacesSquaredOffLengthenYSeparately,
  )where
import CornerPoints.Create(Origin(..), createCornerPointSquaredOff, createCornerPoint)
import CornerPoints.CornerPoints(CornerPoints(..), (+++>), (+++), (|+++|), (|@+++#@|))
import CornerPoints.Points(Point(..))
import CornerPoints.Radius(Radius(..))
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.Transpose (transposeZ, transposeY)
--import CornerPoints.Composable(createCornerPoint)
import CornerPoints.Slope(addSlope)

import Geometry.Radius(squaredOff)
import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..))
import Geometry.Slope(Slope(..), flatXSlope, flatYSlope, slopeAdjustedForVerticalAngle)

type Thickness = Double
type Height = Double
type Power = Double
type LengthenFactor = Double



{- |Create a single CornerPoints.BottomRightLine, and add it to the head of [BottomLeftLine]. This will give a BottomFace.
Then add the next BottomLeftLine to this BottomFace to get the next BottomFace. Continue through the [BottomLeftLine] till
the [BottomFace] is done.

All Lines are created from the inputs, using Polar math.
QuadrantAngle
-}


{-
Replace createCornerPoint with Com.createCornerPoint:

test/HorizontalFacesTest.hs
-recreated the createBottomFaces test. Orig test needs to be removed.

src/Primitives/Cylindrical/Walled.hs:
-uses it in cylinder
 -cylinder is used in Examples/OpenBionicsCom/OpenBionicsDotComDesignWork.hs:35:38: joinerShaft
 -converted to cylinder'

src/Primitives/Cylindrical/Solid.hs:
-converted to createBottomFaces'
-uses it for cylinder which is used by:
 -Examples/Scan/WalkerSocketDesignWork.hs:32:37: to create hose plate.
  -therefore hose plate now used ' version
-slopedTopCylinder which is used in:
 -src/Examples/Primitives/Cylinders.hs: now uses '
  -uses it for slopedToppedCylinder
 -src/Examples/ShoeLift/SnowBoardBoot.hs:
 -treadRearSlopedCubes
 -bootCubes
 
src/Examples/OpenBionicsCom/OpenBionicsDotComDesignWork.hs: uses it in sevaral places
-wristToLargeShaft: now uses '
-wristToSmallShaft: now uses '

src/Primitives/Cubical.hs
-uses it for: rectangularSolidNoSlope
 -now uses '
 -which uses it in src/Examples/Primitives/Cube.hs:rectangle
  -and so now uses ' via rectangle

src/Examples/Primitives/Squared.hs:
-cylinderWithSquaredRadii: now uses '

Examples.Diffs.MTLDiff
-now uses '
-}
createBottomFaces :: Origin -> [Radius] -> [Angle] -> {-Slope -> Slope ->-} [CornerPoints]
createBottomFaces inOrigin radii angles {-xSlope ySlope-}  =
    (createCornerPoint
      (F4)
      inOrigin
      (head radii)
      (head angles)
      
    ) 
    +++
    B4 inOrigin
    +++>
    [(createCornerPoint
      (F1)
      inOrigin
      radius
      angle
      
     ) 
     +++
     B1 inOrigin
       | angle <- tail angles
       | radius <- tail radii
    ]

createBottomFacesSloped :: Origin -> [Radius] -> [Angle] ->  Slope -> Slope -> [CornerPoints]
createBottomFacesSloped inOrigin radii angles xSlope ySlope  =
    (addSlope xSlope ySlope (head angles) inOrigin $
     createCornerPoint
      (F4)
      inOrigin
      (head radii)
      (head angles)
      
    ) 
    +++
    B4 inOrigin
    +++>
    [(addSlope xSlope ySlope (head angles) inOrigin $
      createCornerPoint
      (F1)
      inOrigin
      radius
      angle
      
     ) 
     +++
     B1 inOrigin
       | angle <- tail angles
       | radius <- tail radii
    ]

{- |
Create [BottomFaces] that have modified radii that are squared off by the Geometry.Radius.squaredOff function.
-}
createBottomFacesSquaredOff :: Origin -> [Radius] -> [Angle] ->  Power -> [CornerPoints]
createBottomFacesSquaredOff    inOrigin  radii       angles      power  =
  let radiiSquared =
        [
          squaredOff power radius' angle'
           | radius' <- radii
           | angle'  <- angles
        ]
  in
    (createCornerPoint
      (F4)
      inOrigin
      (head radiiSquared)
      (head angles)
    ) 
    +++
    B4 inOrigin
    +++>
    [(createCornerPoint 
      (F1)
      inOrigin
      radius
      angle
     ) 
     +++
     B1 inOrigin
       | angle <- tail angles
       | radius <- tail radiiSquared
    ]

createBottomFacesWithVariableSlope :: Origin -> [Radius] -> [Angle] -> [Slope] -> [Slope] -> [CornerPoints]
createBottomFacesWithVariableSlope inOrigin inRadius inAngles xSlope ySlope  =
    (addSlope
      (head xSlope)
      (head ySlope)
      (head inAngles)
      inOrigin $
      createCornerPoint
       (F4)
       inOrigin
       (head inRadius) 
       (head inAngles)
       
    ) 
    +++
    B4 inOrigin
    +++>
    [(addSlope
        currXSlope
        currYSlope
        angle
        inOrigin $
        createCornerPoint
         (F1)
         inOrigin
         currRadius
         angle
         
     ) 
     +++
     B1 inOrigin
       | angle <- tail inAngles
       | currRadius <- tail inRadius
       | currXSlope <- tail xSlope
       | currYSlope <- tail ySlope
    ]

{------------------------------------------------------------------ createTopFaces ----------------------------

-}
createTopFaces :: Origin -> [Radius] -> [Angle] -> [CornerPoints]
createTopFaces inOrigin radii angles   =
    (createCornerPoint
      (F3)
      inOrigin
      (head radii)
      (head angles)
      
    ) 
    +++
    B3 inOrigin
    +++>
    [(createCornerPoint
      (F2)
      inOrigin
      radius
      angle
      
     ) 
     +++
     B2 inOrigin
       | angle <- tail angles
       | radius <- tail radii
    ]

createTopFacesSloped :: Origin -> [Radius] -> [Angle] -> Slope -> Slope -> [CornerPoints]
createTopFacesSloped inOrigin radii angles xSlope ySlope =
   (addSlope xSlope ySlope (head angles) inOrigin $
    createCornerPoint
      (F3)
      inOrigin
      (head radii)
      (head angles)
      
    ) 
    +++
    B3 inOrigin
    +++>
    [ (addSlope xSlope ySlope angle inOrigin $
      createCornerPoint
      (F2)
      inOrigin
      radius
      angle
      
     ) 
     +++
     B2 inOrigin
       | angle <- tail angles
       | radius <- tail radii
    ]


createTopFacesWithVariableSlope :: Origin -> [Radius] -> [Angle] -> [Slope] -> [Slope] -> [CornerPoints]
createTopFacesWithVariableSlope    origin    radii        angles    xSlopes      ySlopes  =
    (addSlope
      (head xSlopes)
      (head ySlopes)
      (head angles)
      origin $
      createCornerPoint
       (F3)
       origin
       (head radii) 
       (head angles)
      
    ) 
    +++
    B3 origin
    +++>
    [(addSlope
       currXSlope
       currYSlope
       currAngle
       origin $
       createCornerPoint
        (F2)
        origin
        currRadius
        currAngle
      
     ) 
     +++
     B2 origin
       | currAngle <- tail angles
       | currRadius <- tail radii
       | currXSlope <- tail xSlopes
       | currYSlope <- tail ySlopes
    ]



{-
Used to expand a radial shape along the y-axis. Expands away from the origin to keep it centered.
-}
createBottomFacesLengthenY :: Origin -> [Radius] -> [Angle] -> {-Slope -> Slope ->-} LengthenFactor -> [CornerPoints]
createBottomFacesLengthenY inOrigin radii angles {-xSlope ySlope-} lengthenFactor =
  let createRightLine (Angle angle') cube
        | angle' <= 90 =
            F1 (transposeY ((+)(negate $ lengthenFactor/2)) $ f1 cube)
            +++ (B1 $ b1 cube)
        | angle' <= 270 =
            F1 (transposeY (+(lengthenFactor/2)) $ f1 cube)
            +++ (B1 $ b1 cube)
        | otherwise =
            F1 (transposeY ((+)(negate $lengthenFactor/2)) $ f1 cube)
            +++ (B1 $ b1 cube)
  in
    
     (transposeY ((+)(negate $ lengthenFactor/2))
      (createCornerPoint
        (F4)
        inOrigin
        (head radii)
        (head angles)
        --xSlope
        --ySlope
      )
     )
      +++
      B4 inOrigin
     +++>
     [ createRightLine angle
       (
        (createCornerPoint
         (F1)
          inOrigin
          radius
          angle
          --xSlope
          --ySlope
        ) 
        +++
        B1 inOrigin
       )
      
       | angle <- tail angles
       | radius <- tail radii
     ]


{- |
Used to square off and expand a radial shape along the y-axis. Expands away from the origin, along the y-axis, to keep it centered.

To expand it away from the orign at separate rates see:
createBottomFacesSquaredOffLengthenYSeparately
-}
createBottomFacesSquaredOffLengthenY :: Origin -> [Radius] -> [Angle] ->  Power -> LengthenFactor  -> [CornerPoints]
createBottomFacesSquaredOffLengthenY    inOrigin  radii       angles      power    lengthenFactor  =
  createBottomFacesSquaredOffLengthenYSeparately inOrigin  radii       angles      power    lengthenFactor lengthenFactor

{- |
Used to expand a radial shape along the y-axis. Expands away from the origin to keep it centered like createBottomFacesSquaredOffLengthenY,
except that the amount it moves in the pos and neg y directions, is controlled separately.
-}
createBottomFacesSquaredOffLengthenYSeparately :: Origin -> [Radius] -> [Angle] -> Power -> LengthenFactor -> LengthenFactor  -> [CornerPoints]
createBottomFacesSquaredOffLengthenYSeparately inOrigin radii angles power lengthenNegYFactor lengthenPosYFactor =
  let squaredRadii = [
                       squaredOff power radius' angle'
                       | radius' <- radii
                       | angle' <- angles
                     ]
    
      createLeftLine (Angle angle') cube
        | angle' <= 90 =
            F1 (transposeY ((+)(negate $ lengthenNegYFactor/2)) $ f1 cube)
            +++ (B1 $ b1 cube)
        | angle' <= 270 =
            F1 (transposeY (+(lengthenPosYFactor/2)) $ f1 cube)
            +++ (B1 $ b1 cube)
        | otherwise =
            F1 (transposeY ((+)(negate $lengthenNegYFactor/2)) $ f1 cube)
            +++ (B1 $ b1 cube)
  in
    
     (transposeY ((+)(negate $ lengthenNegYFactor/2))
      (createCornerPoint
        (F4)
        inOrigin
        (head squaredRadii)
        (head angles)
        
      )
     )
      +++
      B4 inOrigin
     +++>
     [ createLeftLine angle
       (
        (createCornerPoint
         (F1)
          inOrigin
          radius
          angle
          
        ) 
        +++
        B1 inOrigin
       )
      
       | angle <- tail angles
       | radius <- tail squaredRadii
     ]
