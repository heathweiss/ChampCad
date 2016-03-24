{-# LANGUAGE TemplateHaskell, ParallelListComp #-} 
module Examples.ShoeLift.GeoxShoe where

import CornerPoints.Radius(Radius(..), buildSymmetricalRadius)
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces)
import CornerPoints.Points(Point(..))
import CornerPoints.Create(Angle(..), flatXSlope, flatYSlope, Slope(..))
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|), DegreeRange(..))
import CornerPoints.CornerPoints((|@+++#@|), (|+++|), CornerPoints(..), (+++), (+++>))
import CornerPoints.Transpose(transposeZ, transposeY)
import CornerPoints.FaceExtraction(extractTopFace, extractBottomFace, extractFrontFace)
import CornerPoints.FaceConversions(upperFaceFromLowerFace, backFaceFromFrontFace )
import CornerPoints.Degree(Degree(..))
import CornerPoints.Transposable(transpose, transposeX)

import Control.Lens


import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..), (+++^))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import Stl.StlCornerPointsWithDegrees(FacesWithRange(..))

import Math.Trigonometry (sinDegrees, cosDegrees)
{-
Shoe lift for the geox running shoes. Refering to the shoe tread will be done with 'geox'
The bottom tread is of a Cougar shoe. Refering to the bottom tread from the Cougar shoe is done with 'cougar'.

The lift has no gaps between the heel and the toe. It is done in 3 sections:
1: The heel, which goes from the back of the shoe, to about the center, which is the back of the flexible section requied when walking.
   Made of solid filament such as pla.
2: The center section which needs to flex while walking. This is made of ninjaflex filament.
3: The toe, which is made of solid filament such as pla.

Lift doesn't use the radial input system because it has to have a variable z-slope, so that the resulting lift requires no vertical shaping to fit.
Is input as a series of cubes running from back to front of the lift.
These cubes are split into 3 groups to correspond with the 3 printable section: solid heel, flexible middle, solid toe

Input for the cubes requires a new datatype that takes a single point and a width. There is 2 arrays of these, 1 for top faces(geox), 1 for bottom faces(cougar).
Much like the radial system, starts with a single back(top/bottom) face, which is then merged into remainder of list for front(top/bottom) faces.
Then zip together the top and bottom to create the lift.
-}




{----------------------------------------------------------- common datatypes, functions and values -------------------------------------}
{-
datatype to handle the basic input. Setup to use Lens package.
leftPoint: A point with represents a CornerPoints(F1,F2,B1,B2).
width: The X-axis offset from the point. Allows for the creation of a F3,F4,B3,B4
-}
--data CubicalInput = CubeIn {_leftPoint::Point, _width::Double}
data CubicalInput = CubeIn {_cornerPoint::CornerPoints, _width::Double}
makeLenses ''CubicalInput

{-
10
15: toe to low
-}
topSlope = 10.0

geoxOrigin = Point 50.2 0 28.1

heightAdj = 80
{-Using the left point, and a width, create a right point-}
createRightPoint :: CubicalInput -> CornerPoints
createRightPoint (CubeIn (B1(point')) width'') =
        (B4
         (transposeX
          (+ width'')
          (point') 
         )
       )

createRightPoint (CubeIn (F1(point')) width'') =
        (F4
         (transposeX
          (+ width'')
          (point') 
         )
       )

createRightPoint (CubeIn (B2(point')) width'') =
        (B3
         (transposeX
          (+ width'')
          (point') 
         )
       )

createRightPoint (CubeIn (F2(point')) width'') =
        (F3
         (transposeX
          (+ width'')
          (point') 
         )
       )

createLine :: CubicalInput -> CornerPoints
createLine (CubeIn (B1(point')) width'') =
  (B1 point')
  +++
  createRightPoint (CubeIn (B1(point')) width'')

createLine (CubeIn (F1(point')) width'') =
  (F1 point')
  +++
  createRightPoint (CubeIn (F1(point')) width'')

createLine (CubeIn (B2(point')) width'') =
  (B2 point')
  +++
  createRightPoint (CubeIn (B2(point')) width'')

createLine (CubeIn (F2(point')) width'') =
  (F2 point')
  +++
  createRightPoint (CubeIn (F2(point')) width'')

------------ adjust for slope
{-Use trig to rotate.
        z' = z - (sin theta y)
        y' = y-origin + (cos theta y)
      -}
adjustTopBase :: Point -> Point -> Double -> Point
adjustTopBase    point    origin   angle  =
        let z = (z_axis point) - (sinDegrees angle * (y_axis point))
            y = (y_axis origin) + (cosDegrees angle * (y_axis point))
        in   Point (x_axis point) y z
        
adjustTop :: Double -> Point ->   CubicalInput   -> CubicalInput
adjustTop    angle     origin    (CubeIn (B2(point')) width')  =
        CubeIn (B2 (adjustTopBase point' origin angle )) width'
adjustTop    angle origin (CubeIn (F2(point')) width')   =
        CubeIn (F2 (adjustTopBase point' origin angle )) width'
      
{--------------------------------------------------------------- Heel section ------------------------------------------------

-}
heel =
  let geoxDimensions =
        [CubeIn (B2(Point 50.2 0 28.1)) 0.0,
         CubeIn (F2(Point 37.5 1 27.7)) 20.4, 
         CubeIn (F2(Point 35.0 3 27.3)) 29.7,
         CubeIn (F2(Point 31.5 5 26.9)) 35.6,
         CubeIn (F2(Point 26.4 10 26.0)) 45.5,
         CubeIn (F2(Point 22.8 15 24.8)) 53.0,
         CubeIn (F2(Point 19.9 20 23.8)) 58.7,
         CubeIn (F2(Point 18.4 25 22.7)) 61.8,
         CubeIn (F2(Point 17.4 30 21.7)) 64.8,
         CubeIn (F2(Point 16.8 35 21.0)) 66.3,
         CubeIn (F2(Point 16.7 40 20.2)) 67.1,
         CubeIn (F2(Point 17.0 45 19.6)) 66.6,
         CubeIn (F2(Point 17.4 50 18.6)) 66.5,
         CubeIn (F2(Point 18.6 55 18.2)) 65.5,
         CubeIn (F2(Point 20.3 60 18.4)) 63.1,
         CubeIn (F2(Point 22.0 65 18.5)) 59.7,
         CubeIn (F2(Point 23.8 70 18.8)) 57.4,
         CubeIn (F2(Point 25.2 75 18.6)) 55.7,
         CubeIn (F2(Point 26.6 80 18.7)) 54.6,
         CubeIn (F2(Point 27.7 85 19.1)) 54.2,
         CubeIn (F2(Point 28.0 90 19.7)) 54.7,
         CubeIn (F2(Point 28.0 95 19.0)) 56.5,
         CubeIn (F2(Point 27.9 100 17.9)) 58.7,
         CubeIn (F2(Point 27.4 105 16.4)) 61.2,
         CubeIn (F2(Point 26.4 110 15.1)) 63.2,
         CubeIn (F2(Point 24.6 115 13.3)) 65.5,
         CubeIn (F2(Point 23.7 120 12.3)) 68.0,
         CubeIn (F2(Point 22.3 125 11.8)) 70.6,
         CubeIn (F2(Point 20.7 130 11.3)) 73.3,
         CubeIn (F2(Point 19.1 135 11.0)) 75.6,
         CubeIn (F2(Point 17.7 140 10.7)) 77.7
        ]
     
      cougarDimensions =
       [CubeIn (B1(Point 50.0 0 34.1)) 0.0,
        CubeIn (F1(Point 45.1 1 34.1)) 10.0, 
        CubeIn (F1(Point 35.0 3 34.1)) 29.9,
        CubeIn (F1(Point 32.0 5 34.1)) 35.9,
        CubeIn (F1(Point 26.1 10 33.9)) 47.4,
        CubeIn (F1(Point 22.3 15 33.4)) 55.5,
        CubeIn (F1(Point 20.2 20 33.0)) 60.4,
        CubeIn (F1(Point 18.1 25 32.1)) 64.6,
        CubeIn (F1(Point 16.7 30 31.5)) 67.7,
        CubeIn (F1(Point 15.9 35 30.6)) 69.7,
        CubeIn (F1(Point 15.3 40 29.6)) 71.0,
        CubeIn (F1(Point 15.1 45 28.5)) 72.0,
        CubeIn (F1(Point 15.1 50 27.8)) 72.9,
        CubeIn (F1(Point 15.4 55 26.7)) 73.2,
        CubeIn (F1(Point 16.0 60 26.0)) 73.2,
        CubeIn (F1(Point 15.9 65 25.2)) 72.9,
        CubeIn (F1(Point 16.8 70 24.2)) 72.2,
        CubeIn (F1(Point 17.0 75 23.4)) 72.3,
        CubeIn (F1(Point 17.7 80 22.4)) 71.7,
        CubeIn (F1(Point 18.3 85 21.8)) 72.5,
        CubeIn (F1(Point 18.8 90 20.8)) 71.3,
        CubeIn (F1(Point 19.2 95 19.2)) 70.7,
        CubeIn (F1(Point 19.5 100 18.1)) 70.9,
        CubeIn (F1(Point 18.8 105 17.0)) 71.9,
        CubeIn (F1(Point 17.7 110 16.0)) 73.3,
        CubeIn (F1(Point 17.0 115 15.0)) 74.3,
        CubeIn (F1(Point 16.2 120 13.8)) 75.8,
        CubeIn (F1(Point 15.7 125 12.9)) 77.2,
        CubeIn (F1(Point 14.4 130 12.1)) 78.7,
        CubeIn (F1(Point 13.9 135 11.8)) 80.4,
        CubeIn (F1(Point 12.8 140 11.8)) 81.8
       ]

      
      
      btm = (createLine (head cougarDimensions))
            +++>
            (map (createLine) (tail cougarDimensions))
          
      slopeAdjustedGeoxDimensions = 
        map (adjustTop topSlope geoxOrigin) geoxDimensions
        
      top = (createLine (head slopeAdjustedGeoxDimensions))
            +++>
            (map (createLine) (tail slopeAdjustedGeoxDimensions))
            
      
      --need to figure out how much height I need
      --this is the original without any slope.
      adjustedTop = map (transposeZ (+heightAdj)) top
        

      heelTriangles = ((FacesAllButFront : [FacesBottomLeftRightTop | x <- [1,2..28]]) ++ [FacesAllButBack])  |+++^| (btm |+++| adjustedTop)
      
  in writeStlToFile $ newStlShape "geox heel" heelTriangles

{-------------------------------------------------------------- center ninja-flex ---------------------------------------------------}

center =
  let geoxDimensions =
        [CubeIn (B2(Point 17.7 140 10.7)) 77.9,
         CubeIn (F2(Point 16.8 145 10.2)) 79.0,
         CubeIn (F2(Point 16.1 150 10.1)) 79.9,
         CubeIn (F2(Point 15.4 155 10.0)) 80.7,
         CubeIn (F2(Point 14.4 160 10.3)) 81.3,
         CubeIn (F2(Point 13.6 165 10.3)) 81.6,
         CubeIn (F2(Point 12.8 170 10.3)) 81.6,
         CubeIn (F2(Point 12.6 175 10.3)) 80.7,
         CubeIn (F2(Point 12.5 180 11.2)) 79.9
        ]

      cougarDimensions =
       [CubeIn (B1(Point 13.1 140 11.8)) 81.8,
        CubeIn (F1(Point 11.7 145 11.7)) 82.8,
        CubeIn (F1(Point 11.3 150 11.7)) 83.8,
        CubeIn (F1(Point 11.0 155 11.7)) 84.5,
        CubeIn (F1(Point 11.0 160 12.2)) 84.5,
        CubeIn (F1(Point 11.0 165 13.3)) 84.6,
        CubeIn (F1(Point 11.8 170 14.8)) 83.7,
        CubeIn (F1(Point 12.2 175 16.0)) 82.5,
        CubeIn (F1(Point 13.2 180 17.5)) 80.4
       ]

      slopeAdjustedGeoxDimensions = 
        map (adjustTop topSlope geoxOrigin) geoxDimensions

      btm = (createLine (head cougarDimensions))
            +++>
            (map (createLine) (tail cougarDimensions))
          

      top = (createLine (head slopeAdjustedGeoxDimensions))
            +++>
            (map (createLine) (tail slopeAdjustedGeoxDimensions))
            
      
      --need to figure out how much height I need
      adjustedTop = map (transposeZ (+heightAdj)) top

      heelTriangles = ((FacesAllButFront : [FacesBottomLeftRightTop | x <- [1,2..6]]) ++ [FacesAllButBack])  |+++^| (btm |+++| adjustedTop)
      
  in writeStlToFile $ newStlShape "geox center" heelTriangles

{---------------------------------------------------------- toe ---------------------------------------------------------------------}

toe =
  let geoxDimensions =
        [CubeIn (B2(Point 12.5 180 11.2)) 79.9,
         CubeIn (F2(Point 12.5 185 11.6)) 78.4,
         CubeIn (F2(Point 12.1 190 12.5)) 77.3,
         CubeIn (F2(Point 11.9 195 13.9)) 76.2,
         CubeIn (F2(Point 12.4 200 15.2)) 73.8,
         CubeIn (F2(Point 13.6 205 16.8)) 69.8,
         CubeIn (F2(Point 15.2 210 18.4)) 65.2,
         CubeIn (F2(Point 17.4 215 20.9)) 58.5,
         CubeIn (F2(Point 20.9 220 23.2)) 49.2,
         CubeIn (F2(Point 26.4 225 25.6)) 36.8,
         CubeIn (F2(Point 39.0 230 28.7)) 14.3,
         CubeIn (F2(Point 47.2 231 28.9)) 0.0
        ]

      cougarDimensions =
       [CubeIn (B1(Point 13.4 180 17.5)) 80.4,
        CubeIn (F1(Point 14.6 185 18.8)) 75.9,
        CubeIn (F1(Point 16.4 190 20.4)) 71.3,
        CubeIn (F1(Point 18.5 195 22.0)) 66.4,
        CubeIn (F1(Point 20.8 200 23.7)) 61.4,
        CubeIn (F1(Point 24.3 205 25.7)) 55.0,
        CubeIn (F1(Point 28.2 210 27.7)) 47.6,
        CubeIn (F1(Point 33.5 215 29.9)) 37.7,
        CubeIn (F1(Point 41.6 220 32.0)) 24.3,
        CubeIn (F1(Point 51.3 223 34.0)) 0.0,
        CubeIn (F1(Point 51.3 223 34.0)) 0.0,
        CubeIn (F1(Point 51.3 223 34.0)) 0.0
       ]

      slopeAdjustedGeoxDimensions = 
        map (adjustTop topSlope geoxOrigin) geoxDimensions

      btm = (createLine (head cougarDimensions))
            +++>
            (map (createLine) (tail cougarDimensions))
          

      top = (createLine (head slopeAdjustedGeoxDimensions))
            +++>
            (map (createLine) (tail slopeAdjustedGeoxDimensions))
            
      
      --need to figure out how much height I need
      adjustedTop = map (transposeZ (+heightAdj)) top

      heelTriangles = ((FacesAllButFront : [FacesBottomLeftRightTop | x <- [1,2..9]]) ++ [FacesAllButBack])  |+++^| (btm |+++| adjustedTop)
      
  in writeStlToFile $ newStlShape "geox toe" heelTriangles


{-keep the old dimensions for now. Thes are before the cut-out z-profiles

heel =
  let geoxDimensions =
        [CubeIn (B2(Point 50.2 0 16.9)) 0.0,
         CubeIn (F2(Point 37.5 1 15.9)) 20.4, 
         CubeIn (F2(Point 35.0 3 14.7)) 29.7,
         CubeIn (F2(Point 31.5 5 14.1)) 35.6,
         CubeIn (F2(Point 26.4 10 12.9)) 45.5,
         CubeIn (F2(Point 22.8 15 11.7)) 53.0,
         CubeIn (F2(Point 19.9 20 11.1)) 58.7,
         CubeIn (F2(Point 18.4 25 10.7)) 61.8,
         CubeIn (F2(Point 17.4 30 9.9)) 64.8,
         CubeIn (F2(Point 16.8 35 9.6)) 66.3,
         CubeIn (F2(Point 16.7 40 9.3)) 67.1,
         CubeIn (F2(Point 17.0 45 10.0)) 66.6,
         CubeIn (F2(Point 17.4 50 10.2)) 66.5,
         CubeIn (F2(Point 18.6 55 10.2)) 65.5,
         CubeIn (F2(Point 20.3 60 9.8)) 63.1,
         CubeIn (F2(Point 22.0 65 10.1)) 59.7,
         CubeIn (F2(Point 23.8 70 10.6)) 57.4,
         CubeIn (F2(Point 25.2 75 11.1)) 55.7,
         CubeIn (F2(Point 26.6 80 11.4)) 54.6,
         CubeIn (F2(Point 27.7 85 12.2)) 54.2,
         CubeIn (F2(Point 28.0 90 13.5)) 54.7,
         CubeIn (F2(Point 28.0 95 14.0)) 56.5,
         CubeIn (F2(Point 27.9 100 14.1)) 58.7,
         CubeIn (F2(Point 27.4 105 13.4)) 61.2,
         CubeIn (F2(Point 26.4 110 12.5)) 63.2,
         CubeIn (F2(Point 24.6 115 11.4)) 65.5,
         CubeIn (F2(Point 23.7 120 10.5)) 68.0,
         CubeIn (F2(Point 22.3 125 10.2)) 70.6,
         CubeIn (F2(Point 20.7 130 10.0)) 73.3,
         CubeIn (F2(Point 19.1 135 9.9)) 75.6,
         CubeIn (F2(Point 17.7 140 10.3)) 77.7
        ]
     
      cougarDimensions =
       [CubeIn (B1(Point 50.0 0 27.5)) 0.0,
        CubeIn (F1(Point 45.1 1 27.6)) 10.0, 
        CubeIn (F1(Point 35.0 3 28.4)) 29.9,
        CubeIn (F1(Point 32.0 5 29.0)) 35.9,
        CubeIn (F1(Point 26.1 10 28.7)) 47.4,
        CubeIn (F1(Point 22.3 15 28.7)) 55.5,
        CubeIn (F1(Point 20.2 20 28.8)) 60.4,
        CubeIn (F1(Point 18.1 25 28.2)) 64.6,
        CubeIn (F1(Point 16.7 30 27.3)) 67.7,
        CubeIn (F1(Point 15.9 35 26.9)) 69.7,
        CubeIn (F1(Point 15.3 40 25.6)) 71.0,
        CubeIn (F1(Point 15.1 45 25.0)) 72.0,
        CubeIn (F1(Point 15.1 50 23.8)) 72.9,
        CubeIn (F1(Point 15.4 55 22.7)) 73.2,
        CubeIn (F1(Point 16.0 60 22.2)) 73.2,
        CubeIn (F1(Point 15.9 65 20.8)) 72.9,
        CubeIn (F1(Point 16.8 70 19.9)) 72.2,
        CubeIn (F1(Point 17.0 75 18.2)) 72.3,
        CubeIn (F1(Point 17.7 80 17.1)) 71.7,
        CubeIn (F1(Point 18.3 85 15.2)) 72.5,
        CubeIn (F1(Point 18.8 90 14.1)) 71.3,
        CubeIn (F1(Point 19.2 95 13.3)) 70.7,
        CubeIn (F1(Point 19.5 100 12.2)) 70.9,
        CubeIn (F1(Point 18.8 105 9.9)) 71.9,
        CubeIn (F1(Point 17.7 110 8.8)) 73.3,
        CubeIn (F1(Point 17.0 115 7.7)) 74.3,
        CubeIn (F1(Point 16.2 120 6.9)) 75.8,
        CubeIn (F1(Point 15.7 125 6.4)) 77.2,
        CubeIn (F1(Point 14.4 130 5.9)) 78.7,
        CubeIn (F1(Point 13.9 135 6.0)) 80.4,
        CubeIn (F1(Point 12.8 140 6.0)) 81.8
       ]

center =
  let geoxDimensions =
        [CubeIn (B2(Point 17.7 140 10.1)) 77.9,
         CubeIn (F2(Point 16.8 145 10.5)) 79.0,
         CubeIn (F2(Point 16.1 150 11.6)) 79.9,
         CubeIn (F2(Point 15.4 155 12.4)) 80.7,
         CubeIn (F2(Point 14.4 160 13.4)) 81.3,
         CubeIn (F2(Point 13.6 165 14.9)) 81.6,
         CubeIn (F2(Point 12.8 170 15.6)) 81.6,
         CubeIn (F2(Point 12.6 175 17.0)) 80.7,
         CubeIn (F2(Point 12.5 180 18.1)) 79.9
        ]

      cougarDimensions =
       [CubeIn (B1(Point 13.1 140 6.1)) 81.8,
        CubeIn (F1(Point 11.7 145 6.3)) 82.8,
        CubeIn (F1(Point 11.3 150 6.7)) 83.8,
        CubeIn (F1(Point 11.0 155 7.6)) 84.5,
        CubeIn (F1(Point 11.0 160 8.2)) 84.5,
        CubeIn (F1(Point 11.0 165 9.4)) 84.6,
        CubeIn (F1(Point 11.8 170 10.3)) 83.7,
        CubeIn (F1(Point 12.2 175 11.8)) 82.5,
        CubeIn (F1(Point 13.2 180 13.6)) 80.7
       ]

toe =
  let geoxDimensions =
        [CubeIn (B2(Point 12.5 180 17.9)) 79.9,
         CubeIn (F2(Point 12.5 185 19.9)) 78.4,
         CubeIn (F2(Point 12.1 190 21.5)) 77.3,
         CubeIn (F2(Point 11.9 195 23.0)) 76.2,
         CubeIn (F2(Point 12.4 200 25.4)) 75.2,
         CubeIn (F2(Point 13.6 205 28.3)) 71.1,
         CubeIn (F2(Point 15.2 210 31.3)) 65.2,
         CubeIn (F2(Point 17.4 215 33.7)) 58.5,
         CubeIn (F2(Point 20.9 220 35.7)) 49.2,
         CubeIn (F2(Point 26.4 225 38.6)) 36.8,
         CubeIn (F2(Point 39.0 230 41.3)) 14.3,
         CubeIn (F2(Point 47.2 231 41.7)) 0.0
        ]

      cougarDimensions =
       [CubeIn (B1(Point 13.4 180 13.2)) 80.4,
        CubeIn (F1(Point 14.6 185 14.9)) 75.9,
        CubeIn (F1(Point 16.4 190 16.5)) 71.3,
        CubeIn (F1(Point 18.5 195 18.1)) 66.4,
        CubeIn (F1(Point 20.8 200 19.5)) 61.4,
        CubeIn (F1(Point 24.3 205 20.7)) 55.0,
        CubeIn (F1(Point 28.2 210 22.2)) 47.6,
        CubeIn (F1(Point 33.5 215 24.5)) 37.7,
        CubeIn (F1(Point 41.6 220 27.5)) 24.3,
        CubeIn (F1(Point 51.3 223 29.5)) 0.0,
        CubeIn (F1(Point 51.3 223 29.5)) 0.0,
        CubeIn (F1(Point 51.3 223 29.5)) 0.0
       ]

-}
