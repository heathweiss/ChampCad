{-# LANGUAGE TemplateHaskell, ParallelListComp #-} 
module Examples.ShoeLift.GeoxShoe where

import CornerPoints.Radius(Radius(..), buildSymmetricalRadius)
import CornerPoints.HorizontalFaces(createTopFaces)
import CornerPoints.Points(Point(..))
import CornerPoints.Create(Angle(..), flatXSlope, flatYSlope, Slope(..))
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|), DegreeRange(..))
import CornerPoints.CornerPoints((|@+++#@|), (|+++|), CornerPoints(..), (+++), (+++>))
import CornerPoints.FaceExtraction(extractTopFace, extractBottomFace, extractFrontFace, extractBackFace, extractLeftFace, extractRightFace)
import CornerPoints.FaceConversions(upperFaceFromLowerFace, backFaceFromFrontFace, lowerFaceFromUpperFace, rigthtFaceFromLeftFace, leftFaceFromRigthFace,
                                   frontFaceFromBackFace)
import CornerPoints.Degree(Degree(..))

import TypeClasses.Transposable(transposeZ, transposeY)

--external libraries
import qualified Flow as Flw

import Cubical.Cubical (CubicalInput(..), createXaxisLine, zDownSlope, adjustWidth)

import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..), (+++^))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import Stl.StlCornerPointsWithDegrees(FacesWithRange(..))

import Math.Trigonometry (sinDegrees, cosDegrees)

import Test.HUnit
{-
Shoe lift for the geox running shoes. Refering to the shoe tread will be done with 'geox'
The bottom tread is of a Cougar shoe. Refering to the bottom tread from the Cougar shoe is done with 'cougar'.

The lift has no gaps between the heel and the toe. It is done in 3 sections:
1: The heel, which goes from the back of the shoe, to about the center, which is the back of the flexible section requied when walking.
   Made of solid filament such as pla.
2: The center section which needs to flex while walking. This is made of ninjaflex filament.
3: The toe, which is made of solid filament such as pla.

This is the 1st use of the new Cubical.Cubical module, which was developed with this lift.

Post-printing note:
The toe did not flex well. Fixed this by:
Glued the center riser section to the toe riser, then unglued the center from the shoe.
This allowed a greater section of flex for the shoe.
In the future, make a 2 piece lift, with only the forward section of the toe glued to the shoe.

10% infill was much more than required for strength, and added too much weight. Should try 7% next time.
-}

{------------------------------------------------ printing notes: ----------------------------------------------------
Slic3r
Heel Bottom:
Cut it at 30mm on z axis, then flipped on z-axis and y-axis so I can print it out on a flat plane without support material.

Heel Top:
Again cut it a 30, but no need to flip or rotate, as it is already correlty orientated with the flat cut on the bottom.

Toe bottom/top:
Same as heel bottom and top.
-} 


{----------------------------------------------------------- common datatypes, functions and values -------------------------------------}


{-
10
15: toe to low
-}
topSlope = 10.0

geoxOrigin = Point 50.2 0 28.1

--the height from the cougar tread to the very top of the riser
topHeightAdj = 80
--the height from the cougar tread, to the bottom of the attachment section.
--This will leave a section which has geox dimensions from top to bottom so attachment
--ring need only have geox dimensions.
transitionHeight = (\z -> 80)




{-Should be replaced with a trig version for creating thickness of the walls.
Keep it around as it could be handy for other thins.-}
widenAndCenter :: Double -> CornerPoints -> CornerPoints
widenAndCenter adjuster (BackTopLine
                         (Point x' y' z'   ) --B2
                         (Point x'' y'' z'') --B3
                        ) =
  let halfAdjuster = adjuster/2
  in  BackTopLine    (Point(x' - halfAdjuster ) y' z'  ) --B2
                     (Point(x'' + halfAdjuster) y'' z'') --B3

widenAndCenter adjuster (FrontTopLine
                         (Point x' y' z'   ) --F2
                         (Point x'' y'' z'') --F3
                        ) =
  let halfAdjuster = adjuster/2
  in  FrontTopLine
                     (Point(x' - halfAdjuster ) y' z'  ) --F2
                     (Point(x'' + halfAdjuster) y'' z'') --F3

{-For setting back the BackTopLine.
Why not just use transposeY?-}
setBack :: Double -> CornerPoints -> CornerPoints
setBack adjuster (BackTopLine
                         (Point x' y' z'   ) --B2
                         (Point x'' y'' z'') --B3
                 ) =
  (BackTopLine
                         (Point x' (y' - adjuster) z'   ) --B2
                         (Point x'' (y'' - adjuster) z'') --B3
  )

---------------------------------- trig widener ----------------------------------------------------------
{-Get the distance of each axis.
Should take 2 points instead?-}
absDistanceEachAxis :: CornerPoints -> Point
absDistanceEachAxis (TopLeftLine (Point x y z) (Point x' y' z')) =
  let
     xDelta = abs $ x - x'
     yDelta = abs $ y - y'
     zDelta = abs $ z - z'
  in Point xDelta yDelta zDelta

absDistance :: CornerPoints -> Double
absDistance (TopLeftLine (Point x y z) (Point x' y' z')) =
  let delta = absDistanceEachAxis (TopLeftLine (Point x y z) (Point x' y' z'))
      
  in  sqrt ((x_axis delta)*2 + (y_axis delta)*2)

{-The angle of the x axis of the line-}
theta :: CornerPoints  -> Double
theta (TopLeftLine (Point x y z) (Point x' y' z')) =
  let  absDistanceEachAxis' = absDistanceEachAxis (TopLeftLine (Point x y z) (Point x' y' z'))
  in
       ((y_axis absDistanceEachAxis')
        /
        ((x_axis absDistanceEachAxis') + (y_axis absDistanceEachAxis') + (y_axis absDistanceEachAxis'))
       )
       *
       180

beta :: Double -> Double
beta  theta' = 180 - theta'

angleOfSetback :: Double -> Double
angleOfSetback beta' = beta' - 90

yDelta :: Double -> Double -> Double
yDelta    wallThickness angleOfSetback' =
  (sinDegrees angleOfSetback') * wallThickness

xDelta :: Double -> Double -> Double
xDelta    wallThickness angleOfSetback' =
  (cosDegrees angleOfSetback') * wallThickness

{-
Left off before trip to whitelaw:
Will move the back line of first cube straight back for now.
Ready to try transposeLine to move a left/right line except for the problem
as listed in function notes.
-}
{-
ToDo:
The y_axis needs to take into consideration the alignment of F2 and B2.
For a left line:
If B2 is right of F2 on x-axis then move y neg, else move it pos.
For a right line:
Should be opposite of left line
-}
transposeLine :: CornerPoints -> Double -> CornerPoints
transposeLine (TopLeftLine (Point x y z) (Point x' y' z')) wallThickness =
  let angleOfSetback' = angleOfSetback (beta (theta (TopLeftLine (Point x y z) (Point x' y' z')) ))
      xDelta' = xDelta wallThickness angleOfSetback'
      yDelta' = yDelta wallThickness angleOfSetback'
  in  
      (TopLeftLine (Point (x - xDelta') (y - yDelta') z) (Point (x' - xDelta') (y' - yDelta') z'))
  

{--------------------------------------------------------------- Heel section -------------------------------------------------}
{-Builds on ankleBraceNoInput. Uses only CornerPoints.
Will try to use a trig system to build the walls.-}
ankleBraceTrigWidener =
  let
    geoxDims =
        --BackTopLine B2 B3
        --FrontTopLine F2 F3
        --added a width to BackTopLine so trig will work.
        --If no width, should default to -y adjustment?
        [BackTopLine (Point 50.2 0 28.1) (Point 51.2 0 28.1),
         FrontTopLine (Point 37.5 1 27.7) (Point 57.9 1 27.3),
         FrontTopLine (Point 35.0 3 27.3) (Point 64.7 3 27.3),
         FrontTopLine (Point 31.5 5 26.9) (Point 67.1 5 26.9),
         FrontTopLine (Point 26.4 10 26.0) (Point 71.9 10 26.0),
         FrontTopLine (Point 22.8 15 24.8) (Point 75.8 15 24.8),
         FrontTopLine (Point 19.9 20 23.8) (Point 78.6 20 23.8),
         FrontTopLine (Point 18.4 25 22.7) (Point 80.2 25 22.7),
         FrontTopLine (Point 17.4 30 21.7) (Point 82.2 30 21.7),
         FrontTopLine (Point 16.8 35 21.0) (Point 83.1 35 21.0),
         FrontTopLine (Point 16.7 40 20.2) (Point 83.8 40 20.2),
         FrontTopLine (Point 17.0 45 19.6) (Point 83.6 45 19.6),
         FrontTopLine (Point 17.4 50 18.6) (Point 83.9 50 18.6),
         FrontTopLine (Point 18.6 55 18.2) (Point 84.1 55 18.2),
         FrontTopLine (Point 20.3 60 18.4) (Point 83.4 60 18.4),
         FrontTopLine (Point 22.0 65 18.5) (Point 81.7 65 18.5),
         FrontTopLine (Point 23.8 70 18.8) (Point 81.2 70 18.8),
         FrontTopLine (Point 25.2 75 18.6) (Point 80.9 75 18.6),
         FrontTopLine (Point 26.6 80 18.7) (Point 81.2 80 18.7),
         FrontTopLine (Point 27.7 85 19.1) (Point 81.9 85 19.1),
         FrontTopLine (Point 28.0 90 19.7) (Point 82.7 90 19.7),
         FrontTopLine (Point 28.0 95 19.0) (Point 84.5 95 19.0),
         FrontTopLine (Point 27.9 100 17.9) (Point 86.6 100 17.9),
         FrontTopLine (Point 27.4 105 16.4) (Point 88.6 105 16.4),
         FrontTopLine (Point 26.4 110 15.1) (Point 89.6 110 15.1),
         FrontTopLine (Point 24.6 115 13.3) (Point 90.1 115 13.3),
         FrontTopLine (Point 23.7 120 12.3) (Point 91.7 120 12.3),
         FrontTopLine (Point 22.3 125 11.8) (Point 92.9 125 11.8),
         FrontTopLine (Point 20.7 130 11.3) (Point 94.0 130 11.3),
         FrontTopLine (Point 19.1 135 11.0) (Point 94.7 135 11.0),
         FrontTopLine (Point 17.7 140 10.7) (Point 95.4 140 10.7)
        ]
    
    wallThickness = 9.0
    height = 10.0
        
    --build the back cube
    first2Lines = take 2 geoxDims
    firstCube =
      let
         topFace = head ((head first2Lines) +++> (tail first2Lines))
      in topFace
         +++
         (transposeZ (\z -> z - height) $ lowerFaceFromUpperFace topFace)

    --make back wall
    {-
    backWall =
      let backTopLineIn = head geoxDims
          f2XWide =
    -}
         
    firstTriangles = FacesAll +++^ firstCube

    firstCubeTest = TestCase $ assertEqual
        "innerFrontFaceTest"
        (F1 (Point 1 2 3))
        (firstCube)

    ankleBraceTestDo = do
        runTestTT firstCubeTest
  in
    writeStlToFile $ newStlShape "geox" firstTriangles
    --ankleBraceTestDo

{-Creates the ankle brace using on CornerPoints, with no CubicalInput.
The widener system is very lacking. Will continue with ankleBraceTrigWidener
to try a better system for wall creation.-}
ankleBraceNoInput =
  let geoxDims =
        --BackTopLine B2 B3
        --FrontTopLine F2 F3
        [BackTopLine (Point 50.2 0 28.1) (Point 50.2 0 28.1),
         FrontTopLine (Point 37.5 1 27.7) (Point 57.9 1 27.3),
         FrontTopLine (Point 35.0 3 27.3) (Point 64.7 3 27.3),
         FrontTopLine (Point 31.5 5 26.9) (Point 67.1 5 26.9),
         FrontTopLine (Point 26.4 10 26.0) (Point 71.9 10 26.0),
         FrontTopLine (Point 22.8 15 24.8) (Point 75.8 15 24.8),
         FrontTopLine (Point 19.9 20 23.8) (Point 78.6 20 23.8),
         FrontTopLine (Point 18.4 25 22.7) (Point 80.2 25 22.7),
         FrontTopLine (Point 17.4 30 21.7) (Point 82.2 30 21.7),
         FrontTopLine (Point 16.8 35 21.0) (Point 83.1 35 21.0),
         FrontTopLine (Point 16.7 40 20.2) (Point 83.8 40 20.2),
         FrontTopLine (Point 17.0 45 19.6) (Point 83.6 45 19.6),
         FrontTopLine (Point 17.4 50 18.6) (Point 83.9 50 18.6),
         FrontTopLine (Point 18.6 55 18.2) (Point 84.1 55 18.2),
         FrontTopLine (Point 20.3 60 18.4) (Point 83.4 60 18.4),
         FrontTopLine (Point 22.0 65 18.5) (Point 81.7 65 18.5),
         FrontTopLine (Point 23.8 70 18.8) (Point 81.2 70 18.8),
         FrontTopLine (Point 25.2 75 18.6) (Point 80.9 75 18.6),
         FrontTopLine (Point 26.6 80 18.7) (Point 81.2 80 18.7),
         FrontTopLine (Point 27.7 85 19.1) (Point 81.9 85 19.1),
         FrontTopLine (Point 28.0 90 19.7) (Point 82.7 90 19.7),
         FrontTopLine (Point 28.0 95 19.0) (Point 84.5 95 19.0),
         FrontTopLine (Point 27.9 100 17.9) (Point 86.6 100 17.9),
         FrontTopLine (Point 27.4 105 16.4) (Point 88.6 105 16.4),
         FrontTopLine (Point 26.4 110 15.1) (Point 89.6 110 15.1),
         FrontTopLine (Point 24.6 115 13.3) (Point 90.1 115 13.3),
         FrontTopLine (Point 23.7 120 12.3) (Point 91.7 120 12.3),
         FrontTopLine (Point 22.3 125 11.8) (Point 92.9 125 11.8),
         FrontTopLine (Point 20.7 130 11.3) (Point 94.0 130 11.3),
         FrontTopLine (Point 19.1 135 11.0) (Point 94.7 135 11.0),
         FrontTopLine (Point 17.7 140 10.7) (Point 95.4 140 10.7)
        ]

      wallThickness = 9.0  
        
      --keep only the 1st 60(ish) degrees
      first60mm = take 18 geoxDims

      --adjust the y-axis so the outer walls are aligned radially,
      --resulting in a more uniform thichness around the curve at the start..
      first60MaintainThickness =
        [ transposeY (+ yAdjust) dim
         | yAdjust <- [(-wallThickness), (-6), (-4), (-2), (-1)] ++ [0,0..]
         | dim <- first60mm
         ]

      --8 for the inside attachment rings + plus 10 for this 5 perimenter piece.
      first60Widened = map (widenAndCenter (wallThickness * 2)) first60MaintainThickness --first60mm

      

      --first60WidenedAndSetBackHead =
      --  setBack wallThickness (head first60Widened) : tail first60Widened

      innerTopFaces = (head first60mm) +++> (tail first60mm)
      innerCubes =
          (map (lowerFaceFromUpperFace . (transposeZ (\z -> z - 5.0))) innerTopFaces)
          |+++|
          innerTopFaces

      innerTriangles = [FacesAll | x <- [1..]] |+++^| innerCubes

      outerTopFaces = (head first60Widened) +++> (tail first60Widened)
      outerCubes =
          (map (lowerFaceFromUpperFace . (transposeZ (\z -> z - 5.0))) outerTopFaces)
          |+++|
          outerTopFaces

      outerTriangles = [FacesAll | x <- [1..]] |+++^| outerCubes

      
      
      backTriangle =
        [FacesBackBottomFrontTop]
         |+++^|
        [ (
          --(extractFrontFace (head innerCubes))
          ((frontFaceFromBackFace . extractBackFace) (head innerCubes))
          +++
          (extractBackFace (head outerCubes))
         )
        ]

      armFaces = ( [FacesBottomLeftRightTop | x <- [1..16]] ++ [FacesBottomFrontLeftRightTop])

      leftCubes =
        --extract left face of outer tail
        (map (extractLeftFace) (outerCubes) )
        |+++|
        (map (rigthtFaceFromLeftFace . extractLeftFace) (innerCubes))
        --extract left face of inner tail
          --convert to right faces
        --add them together
      leftTriangles = armFaces  |+++^| leftCubes

      rightCubes =
        --extract right faces of outer tail
        (map (extractRightFace) (outerCubes))
        |+++|
        --extract left faces and covert to left faces, inner tail
        (map (leftFaceFromRigthFace . extractRightFace) (innerCubes))

      rightTriangles = armFaces |+++^| rightCubes
      
      innerFrontFaceTest = TestCase $ assertEqual
        "innerFrontFaceTest"
        (F1 (Point 1 2 3))
        (extractFrontFace (head innerCubes))

      outerBackFaceTest = TestCase $ assertEqual
        "outerBackFaceTest"
        (F1 (Point 1 2 3))
        (extractBackFace (head outerCubes))

      backCubeTest = TestCase $ assertEqual
        "backCubeTest"
        (F1 (Point 1 2 3))
        ((extractFrontFace (head innerCubes))
         +++
         (extractBackFace (head outerCubes))
        )

      ankleBraceNoInputTestDo = do
        --runTestTT innerFrontFaceTest
        --runTestTT outerBackFaceTest
        runTestTT backCubeTest
     
  in writeStlToFile $ newStlShape "geox" $ backTriangle  ++ leftTriangles ++ rightTriangles
     --ankleBraceNoInputTestDo



  
ankleBrace =
  let geoxDims =
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
      
      --keep only the 1st 60(ish) degrees
      geoxFirst60Degrees = take 18 geoxDims

      --8 for the inside attachment rings + plus 10 for this 5 perimenter piece.
      geoxDimsWidened = map (adjustWidth (+18.0)) geoxFirst60Degrees

      --give it a flat bottom, with height adjusted
      geoxDimsWidenedAndFlatened = map (transposeZ (\z -> (45))) geoxDimsWidened

      topOfBtmLayerFaces =
        (createXaxisLine (head geoxDimsWidenedAndFlatened))
        +++>
        (map (createXaxisLine) (tail geoxDimsWidenedAndFlatened))

      

      

      ankleTriangles =
        topOfBtmLayerFaces
        Flw.|> (\topFaces ->
                 let 
                     btmFaces =
                       map ((transposeZ (\z -> 0.0)) . lowerFaceFromUpperFace) topFaces
                     
                     btmCubes = btmFaces |+++| topFaces
                     btmTriangles =
                       ((FacesBackBottomLeftRight : [FacesBottomLeftRight | x <- [1,2..15]]) ++ [FacesBottomFrontLeftRight])
                       |+++^|
                       btmCubes
                 in  (btmTriangles, topFaces)
               )
        Flw.|> (\(triangles', layer1TopFaces) ->
                 let layer2TopFaces = map ( (transposeY (+30)) . (transposeZ (+55.0))) layer1TopFaces
                     layer2Cubes = layer2TopFaces
                                   |+++|
                                   (map (lowerFaceFromUpperFace) layer1TopFaces)
                     layer2Triangles =
                       ((FacesBackLeftRightTop : [FacesLeftRightTop | x <- [1,2..15]]) ++ [FacesFrontLeftRightTop])
                       |+++^|
                       layer2Cubes
                 in (triangles' ++ layer2Triangles )
               )




                 
               
        

  in  writeStlToFile $ newStlShape "geox heel" ankleTriangles

{-Attachment ring that goes over the geoxHeelAttachmentRing to fill out to the diameter required for the anlkle brace..
Is the same shape as the bottom of the ankle brace.-}
geoxHeelRearHalfAttachmentRing = 
  let geoxDims =
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

      --keep only the 1st 60(ish) degrees
      geoxFirst60Degrees = take 18 geoxDims

      --4 for the inside attachment ring + plus 2 for this 2 perimenter piece.
      geoxDimsWidened = map (adjustWidth (+8.0)) geoxFirst60Degrees

      --give it a flat bottom, with height adjusted
      geoxDimsWidenedAndFlatened = map (transposeZ (\z -> (-10))) geoxDimsWidened


      btmGeoxFacesBeforeConversionFromTopFaces = 
        (createXaxisLine (head geoxDimsWidenedAndFlatened))
        +++>
        (map (createXaxisLine) (tail geoxDimsWidenedAndFlatened))

      btmGeoxFaces = map (lowerFaceFromUpperFace) btmGeoxFacesBeforeConversionFromTopFaces

      topGeoxSlopedDims = map (zDownSlope topSlope geoxOrigin) geoxDimsWidened

      topGeoxFaces =
        (createXaxisLine (head topGeoxSlopedDims))
        +++>
        (map (createXaxisLine) (tail topGeoxSlopedDims))

      geoxCubes = btmGeoxFaces |+++| topGeoxFaces

      geoxTriangles = ((FacesBackBottomLeftRightTop : [FacesBottomLeftRightTop | x <- [1,2..15]]) ++ [FacesBottomFrontLeftRightTop]) |+++^| geoxCubes

  in  writeStlToFile $ newStlShape "geox heel" geoxTriangles

{-A thin ring that attaches the heel section of the riser to the geox tread.
Has a flat bottom and same slope as riser.-}
geoxHeelAttachmentRing =
  let geoxDims =
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

      geoxDimsWidened = map (adjustWidth (+4.0)) geoxDims
      geoxDimsWidenedAndFlatened = map (transposeZ (\z -> (-30))) geoxDimsWidened

      btmGeoxFacesBeforeConversionFromTopFaces = 
        (createXaxisLine (head geoxDimsWidenedAndFlatened))
            +++>
            (map (createXaxisLine) (tail geoxDimsWidenedAndFlatened))
      
      btmGeoxFaces = map (lowerFaceFromUpperFace) btmGeoxFacesBeforeConversionFromTopFaces

      topGeoxSlopedDims = map (zDownSlope topSlope geoxOrigin) geoxDimsWidened

      topGeoxFaces =
        (createXaxisLine (head topGeoxSlopedDims))
        +++>
        (map (createXaxisLine) (tail topGeoxSlopedDims))

      geoxCubes = btmGeoxFaces |+++| topGeoxFaces

      geoxTriangles = ((FacesBackBottomLeftRightTop : [FacesBottomLeftRightTop | x <- [1,2..28]]) ++ [FacesBottomFrontLeftRightTop]) |+++^| geoxCubes
        
     
  in  writeStlToFile $ newStlShape "geox heel" geoxTriangles

{-
Ring used to attach the cougar tread to the riser.
Widen the cougar dimenstions and give them a flat set of top faces for easy printing when inverted.
Print it out without any infill should give me a nice ring.
-}
cougarHeelAttachmentRing =
  let
    cougarDimensions =
       [CubeIn (B1(Point 46.3 0 68.1)) 0.0,
        CubeIn (F1(Point 43.5 1 67.7)) 10.0, 
        CubeIn (F1(Point 36.2 3 67.3)) 28.7,
        CubeIn (F1(Point 30.5 5 67.1)) 35.7,
        CubeIn (F1(Point 25.2 10 66.7)) 45.9,
        CubeIn (F1(Point 22.1 15 65.5)) 53.4,
        CubeIn (F1(Point 18.9 20 65.1)) 59.8,
        CubeIn (F1(Point 16.8 25 65.1)) 64.2,
        CubeIn (F1(Point 15.0 30 64.1)) 66.8,
        CubeIn (F1(Point 14.3 35 64.1)) 68.7,
        CubeIn (F1(Point 14.8 40 64.4)) 69.6,
        CubeIn (F1(Point 14.8 45 64.7)) 70.2,
        CubeIn (F1(Point 15.0 50 64.7)) 71.3,
        CubeIn (F1(Point 15.0 55 64.8)) 71.3,
        CubeIn (F1(Point 15.7 60 64.8)) 71.5,
        CubeIn (F1(Point 15.9 65 63.8)) 71.4,
        CubeIn (F1(Point 16.3 70 63.3)) 70.7,
        CubeIn (F1(Point 16.7 75 62.9)) 70.3,
        CubeIn (F1(Point 17.3 80 62.0)) 70.3,
        CubeIn (F1(Point 18.2 85 60.8)) 69.2,
        CubeIn (F1(Point 18.9 90 59.4)) 68.6,
        CubeIn (F1(Point 18.7 95 58.4)) 69.4,
        CubeIn (F1(Point 18.7 100 57.4)) 69.4,
        CubeIn (F1(Point 18.0 105 55.6)) 70.7,
        CubeIn (F1(Point 17.3 110 54.8)) 72.4,
        CubeIn (F1(Point 16.9 115 53.8)) 72.7,
        CubeIn (F1(Point 15.8 120 52.7)) 74.7,
        CubeIn (F1(Point 15.2 125 52.1)) 75.8,
        CubeIn (F1(Point 14.6 130 51.1)) 77.6,
        CubeIn (F1(Point 13.5 135 51.3)) 79.4,
        CubeIn (F1(Point 12.4 140 50.3)) 81.4
       ]

    

    cougarDimensionsWidened = map (adjustWidth (+4)) cougarDimensions
    
    btmFaces = (createXaxisLine (head cougarDimensionsWidened))
            +++>
            (map (createXaxisLine) (tail cougarDimensionsWidened))
    
    
      
    topDimensionsWithFlatTop = map (transposeZ (\z -> 88)) cougarDimensionsWidened

    topFacesBeforeConversionFromBottomFaces =
      (createXaxisLine (head topDimensionsWithFlatTop))
      +++>
      (map (createXaxisLine) (tail topDimensionsWithFlatTop))

    topFaces = map (upperFaceFromLowerFace) topFacesBeforeConversionFromBottomFaces

    heelCubes = btmFaces |+++| topFaces

    heelTriangles = ((FacesAllButFront : [FacesBottomLeftRightTop | x <- [1,2..28]]) ++ [FacesBottomFrontLeftRightTop]) |+++^| heelCubes
    
  in
     writeStlToFile $ newStlShape "geox heel" heelTriangles

heelRiser =
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
       [CubeIn (B1(Point 46.3 0 34.1)) 0.0,
        CubeIn (F1(Point 43.5 1 34.1)) 10.0, 
        CubeIn (F1(Point 36.2 3 34.1)) 28.7,
        CubeIn (F1(Point 30.5 5 34.1)) 35.7,
        CubeIn (F1(Point 25.2 10 33.9)) 45.9,
        CubeIn (F1(Point 22.1 15 33.4)) 53.4,
        CubeIn (F1(Point 18.9 20 33.0)) 59.8,
        CubeIn (F1(Point 16.8 25 32.1)) 64.2,
        CubeIn (F1(Point 15.0 30 31.5)) 66.8,
        CubeIn (F1(Point 14.3 35 30.6)) 68.7,
        CubeIn (F1(Point 14.8 40 29.6)) 69.6,
        CubeIn (F1(Point 14.8 45 28.5)) 70.2,
        CubeIn (F1(Point 15.0 50 27.8)) 71.3,
        CubeIn (F1(Point 15.0 55 26.7)) 71.3,
        CubeIn (F1(Point 15.7 60 26.0)) 71.5,
        CubeIn (F1(Point 15.9 65 25.2)) 71.4,
        CubeIn (F1(Point 16.3 70 24.2)) 70.7,
        CubeIn (F1(Point 16.7 75 23.4)) 70.3,
        CubeIn (F1(Point 17.3 80 22.4)) 70.3,
        CubeIn (F1(Point 18.2 85 21.8)) 69.2,
        CubeIn (F1(Point 18.9 90 20.8)) 68.6,
        CubeIn (F1(Point 18.7 95 19.2)) 69.4,
        CubeIn (F1(Point 18.7 100 18.1)) 69.4,
        CubeIn (F1(Point 18.0 105 17.0)) 70.7,
        CubeIn (F1(Point 17.3 110 16.0)) 72.4,
        CubeIn (F1(Point 16.9 115 15.0)) 72.7,
        CubeIn (F1(Point 15.8 120 13.8)) 74.7,
        CubeIn (F1(Point 15.2 125 12.9)) 75.8,
        CubeIn (F1(Point 14.6 130 12.1)) 77.6,
        CubeIn (F1(Point 13.5 135 11.8)) 79.4,
        CubeIn (F1(Point 12.4 140 11.8)) 81.4
       ]

      {-------------------------- attachment -----------------------
      The section that attaches to the geox tread.
      Has geox x/y dimensions from top to bottom for easy fitting of the attachment ring.
      -}

      attachmentSlopeAdjustedTopGeoxDimensions = 
        map (zDownSlope topSlope geoxOrigin) geoxDimensions
      
      attachmentTopSlopeAdjustedGeoxFaces = (createXaxisLine (head attachmentSlopeAdjustedTopGeoxDimensions))
            +++>
            (map (createXaxisLine) (tail attachmentSlopeAdjustedTopGeoxDimensions))
            
      
      --The top faces that meet the geox tread. Adjusted for height and slope.
      attachmentSlopeAndHeightAdjustedTopGeoxFaces = map (transposeZ (+topHeightAdj)) attachmentTopSlopeAdjustedGeoxFaces

      attachmentCubes = attachmentSlopeAndHeightAdjustedTopGeoxFaces |+++| transitionCubes
      attachmentTriangles = ((FacesLeftRightTop : [FacesLeftRightTop | x <- [1,2..28]]) ++ [FacesFrontLeftRightTop]) |+++^| attachmentCubes
      

      {------------------------ transition -----------------
      Transitions from the dimensions of the cougar tread to the geox tread
      -}
      
      transitionBtmCougarFaces = (createXaxisLine (head cougarDimensions))
            +++>
            (map (createXaxisLine) (tail cougarDimensions))


      {-Has same slope top as goex tread. This is a slope on a constant z-value
        instead of the z as captured to match cougar/geox treads.
        This will give the attachment ring a flat bottom.
      -}
      transitionTopGeoxDimensions = map (transposeZ transitionHeight) geoxDimensions
      --now add a slope to them
      transitionTopGeoxDimensionsSloped = map (zDownSlope topSlope geoxOrigin) transitionTopGeoxDimensions
      --create a set of top faces from them
      transitionSlopedTopGeoxDimensions =
            (createXaxisLine (head transitionTopGeoxDimensionsSloped))
            +++>
            (map (createXaxisLine) (tail transitionTopGeoxDimensionsSloped)) 


      transitionCubes = transitionBtmCougarFaces |+++| transitionSlopedTopGeoxDimensions
      transitionTriangles = ((FacesBackBottomLeftRight : [FacesBottomLeftRight | x <- [1,2..28]]) ++ [FacesBottomFrontLeftRight]) |+++^| transitionCubes
          
      

      
      
  in writeStlToFile $ newStlShape "geox heel" (transitionTriangles ++ attachmentTriangles) 

{-------------------------------------------------------------- center ninja-flex ---------------------------------------------------}

centerCougarAttachmentRing =
  let cougarDimensions =
       [CubeIn (B1(Point 12.8 140 50.5)) 81.4,
        CubeIn (F1(Point 11.1 145 50.5)) 82.4,
        CubeIn (F1(Point 10.5 150 50.5)) 82.8,
        CubeIn (F1(Point 10.4 155 51.1)) 83.4,
        CubeIn (F1(Point 10.4 160 51.1)) 83.4,
        CubeIn (F1(Point 10.4 165 51.8)) 83.4,
        CubeIn (F1(Point 10.4 170 52.9)) 81.9,
        CubeIn (F1(Point 10.8 175 54.4)) 81.2,
        CubeIn (F1(Point 11.2 180 56.0)) 79.6
       ]
      
      
      
      cougarDimensionsWidened = map (adjustWidth (+4)) cougarDimensions
      
      btmFaces = (createXaxisLine (head cougarDimensionsWidened))
            +++>
            (map (createXaxisLine) (tail cougarDimensionsWidened))
      
      
      
      topDimensionsWithFlatTop = map (transposeZ (\z -> 88)) cougarDimensionsWidened

      topFacesBeforeConversionFromBottomFaces =
       (createXaxisLine (head topDimensionsWithFlatTop))
       +++>
       (map (createXaxisLine) (tail topDimensionsWithFlatTop))
      
      topFaces = map (upperFaceFromLowerFace) topFacesBeforeConversionFromBottomFaces
      
      centerCubes = btmFaces |+++| topFaces

      centerTriangles = ((FacesAllButFront : [FacesBottomLeftRightTop | x <- [1,2..6]]) ++ [FacesBottomFrontLeftRightTop]) |+++^| centerCubes
    
  in
     writeStlToFile $ newStlShape "geox heel" centerTriangles
  



centerRiser =
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
       [CubeIn (B1(Point 12.8 140 11.8)) 81.4,
        CubeIn (F1(Point 11.1 145 11.7)) 82.4,
        CubeIn (F1(Point 10.5 150 11.7)) 82.8,
        CubeIn (F1(Point 10.4 155 11.7)) 83.4,
        CubeIn (F1(Point 10.4 160 12.2)) 83.4,
        CubeIn (F1(Point 10.4 165 13.3)) 83.4,
        CubeIn (F1(Point 10.4 170 14.8)) 81.9,
        CubeIn (F1(Point 10.8 175 16.0)) 81.2,
        CubeIn (F1(Point 11.2 180 17.5)) 79.6
       ]

      {-------------------------- attachment -----------------------
      The section that attaches to the geox tread.
      Has geox x/y dimensions from top to bottom for easy fitting of the attachment ring.
      -}

      attachmentSlopeAdjustedTopGeoxDimensions = 
        map (zDownSlope topSlope geoxOrigin) geoxDimensions
      
      attachmentTopSlopeAdjustedGeoxFaces = (createXaxisLine (head attachmentSlopeAdjustedTopGeoxDimensions))
            +++>
            (map (createXaxisLine) (tail attachmentSlopeAdjustedTopGeoxDimensions))
            
      
      --The top faces that meet the geox tread. Adjusted for height and slope.
      attachmentSlopeAndHeightAdjustedTopGeoxFaces = map (transposeZ (+topHeightAdj)) attachmentTopSlopeAdjustedGeoxFaces

      attachmentCubes = attachmentSlopeAndHeightAdjustedTopGeoxFaces |+++| transitionCubes
      attachmentTriangles = ((FacesBackLeftRightTop : [FacesLeftRightTop | x <- [1,2..6]]) ++ [FacesFrontLeftRightTop]) |+++^| attachmentCubes
      

      {------------------------------------------ transition ----------------------------}

      transitionBtmCougarFaces = (createXaxisLine (head cougarDimensions))
            +++>
            (map (createXaxisLine) (tail cougarDimensions))

      {-Has same slope top as goex tread. This is a slope on a constant z-value
        instead of the z as captured to match cougar/geox treads.
        This will give the attachment ring a flat bottom.
      -}
      transitionTopGeoxDimensions = map (transposeZ transitionHeight) geoxDimensions
      
      --now add a slope to them
      transitionTopGeoxDimensionsSloped = map (zDownSlope topSlope geoxOrigin) transitionTopGeoxDimensions
      --create a set of top faces from them
      transitionSlopedTopGeoxDimensions =
            (createXaxisLine (head transitionTopGeoxDimensionsSloped))
            +++>
            (map (createXaxisLine) (tail transitionTopGeoxDimensionsSloped)) 


      transitionCubes = transitionBtmCougarFaces |+++| transitionSlopedTopGeoxDimensions
      transitionTriangles = ((FacesBackBottomLeftRight : [FacesBottomLeftRight | x <- [1,2..6]]) ++ [FacesBottomFrontLeftRight]) |+++^| transitionCubes
          

      --heelTriangles = ((FacesAllButFront : [FacesBottomLeftRightTop | x <- [1,2..6]]) ++ [FacesAllButBack])  |+++^| (transitionBtmCougarFaces |+++| attachmentSlopeAndHeightAdjustedTopGeoxFaces)
      
  in writeStlToFile $ newStlShape "geox center"  (transitionTriangles ++ attachmentTriangles)

{---------------------------------------------------------- toe ---------------------------------------------------------------------}

toeRiser =
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
       [CubeIn (B1(Point 12.5 180 17.5)) 79.9,
        CubeIn (F1(Point 12.4 185 18.8)) 77.1,
        CubeIn (F1(Point 13.6 190 20.4)) 73.3,
        CubeIn (F1(Point 16.3 195 22.0)) 68.6,
        CubeIn (F1(Point 18.0 200 23.7)) 64.3,
        CubeIn (F1(Point 20.2 205 25.7)) 59.1,
        CubeIn (F1(Point 23.0 210 27.7)) 53.1,
        CubeIn (F1(Point 26.9 215 29.9)) 45.9,
        CubeIn (F1(Point 33.1 220 32.0)) 36.3,
        CubeIn (F1(Point 41.6 223 34.0)) 22.5,
        CubeIn (F1(Point 45.8 224 34.8)) 9.4,
        CubeIn (F1(Point 48.5 225 35.2)) 0.0
       ]

      {-------------------------- attachment -----------------------
      The section that attaches to the geox tread.
      Has geox x/y dimensions from top to bottom for easy fitting of the attachment ring.
      -}

      attachmentSlopeAdjustedTopGeoxDimensions = 
        map (zDownSlope topSlope geoxOrigin) geoxDimensions
      
      attachmentTopSlopeAdjustedGeoxFaces = (createXaxisLine (head attachmentSlopeAdjustedTopGeoxDimensions))
            +++>
            (map (createXaxisLine) (tail attachmentSlopeAdjustedTopGeoxDimensions))
            
      
      --The top faces that meet the geox tread. Adjusted for height and slope.
      attachmentSlopeAndHeightAdjustedTopGeoxFaces = map (transposeZ (+topHeightAdj)) attachmentTopSlopeAdjustedGeoxFaces

      attachmentCubes = attachmentSlopeAndHeightAdjustedTopGeoxFaces |+++| transitionCubes
      attachmentTriangles = ((FacesBackLeftRightTop : [FacesLeftRightTop | x <- [1,2..9]]) ++ [FacesFrontLeftRightTop]) |+++^| attachmentCubes
      

      {------------------------------------------ transition ---------------------------
      Transition from the cougar tread dimensions, up to the geox tread dimensions which have been sloped to keep the toe down a bit.
      -}

      transitionBtmCougarFaces = (createXaxisLine (head cougarDimensions))
            +++>
            (map (createXaxisLine) (tail cougarDimensions))

      {-Has same slope top as goex tread. This is a slope on a constant z-value
        instead of the z as captured to match cougar/geox treads.
        This will give the attachment ring a flat bottom.
      -}
      transitionTopGeoxDimensions = map (transposeZ transitionHeight) geoxDimensions
      --now add a slope to them
      transitionSlopedGeoxDimensions = map (zDownSlope topSlope geoxOrigin) transitionTopGeoxDimensions
      --create a set of top faces from them
      transitionSlopedGeoxTopFaces =
            (createXaxisLine (head transitionSlopedGeoxDimensions))
            +++>
            (map (createXaxisLine) (tail transitionSlopedGeoxDimensions)) 


      transitionCubes = transitionBtmCougarFaces |+++| transitionSlopedGeoxTopFaces
      transitionTriangles = ((FacesBackBottomLeftRight : [FacesBottomLeftRight | x <- [1,2..9]]) ++ [FacesBottomFrontLeftRight]) |+++^| transitionCubes
          

      
  in writeStlToFile $ newStlShape "geox toe"  (transitionTriangles ++ attachmentTriangles)

toeTopAttachment =
  let 
      geoxDimensions =
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
      
      --widen cougar dim's to fit around the shoe/riser
      geoxDimensionsWidened = map (adjustWidth (+4.0)) geoxDimensions

      geoxDimensionsWidenedAndFlattened = map (transposeZ (\z -> 40.0)) geoxDimensionsWidened

      geoxTopFaces =
        (createXaxisLine (head geoxDimensionsWidenedAndFlattened))
        +++>
        (map (createXaxisLine) (tail geoxDimensionsWidenedAndFlattened))

       

      geoxCubes =
         geoxTopFaces
         |+++|
         (map ((transposeZ ((-)40)) . lowerFaceFromUpperFace) geoxTopFaces)

      geoxTriangles = ((FacesBackBottomLeftRightTop : [FacesBottomLeftRightTop | x <- [1,2..9]]) ++ [FacesBottomFrontLeftRightTop])  |+++^| geoxCubes

  in writeStlToFile $ newStlShape "geox toe" geoxTriangles

toeBtmAttachment =
  let 
      geoxDimensions =
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
        [CubeIn (B1(Point 12.5 180 17.5)) 79.9,
         CubeIn (F1(Point 12.4 185 18.8)) 77.1,
         CubeIn (F1(Point 13.6 190 20.4)) 73.3,
         CubeIn (F1(Point 16.3 195 22.0)) 68.6,
         CubeIn (F1(Point 18.0 200 23.7)) 64.3,
         CubeIn (F1(Point 20.2 205 25.7)) 59.1,
         CubeIn (F1(Point 23.0 210 27.7)) 53.1,
         CubeIn (F1(Point 26.9 215 29.9)) 45.9,
         CubeIn (F1(Point 33.1 220 32.0)) 36.3,
         CubeIn (F1(Point 41.6 223 34.0)) 22.5,
         CubeIn (F1(Point 45.8 224 34.8)) 9.4,
         CubeIn (F1(Point 48.5 225 35.2)) 0.0
        ]


      {--------------------------------------------------------------------
      Bottom sections that fits to the cougar tread
      -}
      --widen cougar dim's to fit around the shoe/riser
      cougarDimensionsWidened = map (adjustWidth (+4.0)) cougarDimensions
      
      --reduce the height of the cougar dimensions
      cougarDimensionsWidenedAndLowered = map (transposeZ (\z -> z - 6.5)) cougarDimensionsWidened
      --build a set of bottom faces from them
      btmCougarFaces =
            (createXaxisLine (head cougarDimensionsWidenedAndLowered))
            +++>
            (map (createXaxisLine) (tail cougarDimensionsWidenedAndLowered))

      --build a set of top faces from the original dimensions
      topCougarFacesAsBtmFaces =
            (createXaxisLine (head cougarDimensionsWidened))
            +++>
            (map (createXaxisLine) (tail cougarDimensionsWidened))

      topCougarFaces = map (upperFaceFromLowerFace) topCougarFacesAsBtmFaces
      --add them to the new bottoms
      btmCougarCubes = btmCougarFaces |+++| topCougarFaces
      btmCougarTriangles = ((FacesBackBottomLeftRight : [FacesBottomLeftRight | x <- [1,2..9]]) ++ [FacesBottomFrontLeftRight]) |+++^| btmCougarCubes

      {---------------------------------------------------------------
      top section that fits to the riser. Transitions from cougar to geox.
      -}
      --adjust dimensions for width and flattened out for 1 cm height above bottom section
      geoxDimensionsWidened = [adjustWidth widthAdj cubeIn
                               | widthAdj <- ([(+4.0) | x <- [1,2..11]] ++ [(+0.0)])
                               | cubeIn   <- geoxDimensions
                              ]
      geoxDimensionsWidenedAndFlattened = map (transposeZ (\z -> 47.5 ))  geoxDimensionsWidened

      --create top faces
      geoxTopFaces = (createXaxisLine (head geoxDimensionsWidenedAndFlattened))
            +++>
            (map (createXaxisLine) (tail geoxDimensionsWidenedAndFlattened))
      
      geoxCubes = geoxTopFaces |+++| (map (lowerFaceFromUpperFace) topCougarFaces)

      geoxTriangles = ((FacesBackLeftRightTop : [FacesLeftRightTop | x <- [1,2..9]]) ++ [FacesFrontLeftRightTop]) |+++^| geoxCubes

      --create top faces from the sloped geox dimensions
      --add them to the existing cubes

  in writeStlToFile $ newStlShape "geox toe" (btmCougarTriangles ++ geoxTriangles) --  (transitionTriangles ++ attachmentTriangles)

{-


-}
