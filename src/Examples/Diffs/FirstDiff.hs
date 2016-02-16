{-# LANGUAGE TemplateHaskell #-}
module Examples.Diffs.FirstDiff where
import CornerPoints.Radius(Radius(..))
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces)
import CornerPoints.Create(Angle(..), flatXSlope, flatYSlope, Origin(..))
import CornerPoints.CornerPoints((|@+++#@|), (|+++|),  CornerPoints(..), (+++))
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPointsWithDegrees(DegreeRange(..), CornerPointsWithDegrees(..), newCornerPointsWithDegreesList )
import CornerPoints.FaceExtraction(extractFrontFace, extractFrontLeftLine, extractFrontRightLine, extractLeftFace,
                                  extractRightFace, extractBackRightLine)
import CornerPoints.FaceConversions(toBackFace, reverseNormal, toFrontFace, backFaceFromFrontFace)
import CornerPoints.FaceExtractAndConvert(getFrontFaceAsBackFace, getFrontLeftLineAsBackFace, getLeftFaceAsBackFace,
                                          getFrontRightLineAsBackFace, getRightFaceAsBackFace, getBackRightLineAsBackFace)


import Stl.StlCornerPointsWithDegrees(FacesWithRange(..), FacesWithRange(..), (|@~?+++^|) )
import Stl.StlCornerPoints(Faces(..), (+++^))
import Stl.StlFileWriter(writeStlToFile)
import Stl.StlBase (StlShape(..), newStlShape)

import qualified Builder.Sequence as S (newCornerPointsWithDegreesBuilder, (||@~+++^||), (@~+++#@|>), (@~+++@|>))
import qualified Builder.List as L ((||@~+++^||))

import Test.HUnit

import qualified Data.Foldable as F
import qualified Data.Sequence as S

--imports for arrows
import Control.Arrow hiding ((+++))
import Control.Category
import Prelude hiding ((.), id)
import qualified Data.Map as M
import Control.Lens
{-------------------------------------- overview ---------------------------------------
Create a simple radial shape with 40-45-90...360 angles.
Create at origin.

Create as second smaller simple radial shape that is offeset from the origin,
such that it will be located withing 1 triangle of large shape.

Subtract the small shape from the triangle of large shape:

so far:
Have removed the faces of large triangle
Have moved small triangle over to large triangle space, and displayed it as a self contained shape.

next: ??????
-}

{-
create a tuple from [CornerPointsWithDegrees], required to make a map with:
key: DegreeRange
value: CornerPoints
-}
cornerPointsMap = map (\(CubesWithStartEndDegrees cube degreeRange) -> (degreeRange,cube))

{-Extract the CornerPoint from the map Maybe result. Gives CornerPointsError on Nothing -}
extractMaybeCube :: Maybe CornerPoints ->  CornerPoints
extractMaybeCube (Just a) = a
extractMaybeCube Nothing = CornerPointsError "error" -- F1 (Point 1 2 3)



--angles for small/large shapes
angles = map Angle [0,45..360]

{------------------------------------- large shape ------------------------------
Create the large shape, which will have the small shape embedded.-}
largeRadius = [(Radius x) | x <- [20,20..]]

largeShape =
  createBottomFaces (Point 0 0 0) largeRadius angles flatXSlope flatYSlope
  |+++|
  createTopFaces (Point 0 0 10) largeRadius angles flatXSlope flatYSlope

--make the CornerPointsWithDegreesList: 
largeShapeWithDegrees = newCornerPointsWithDegreesList 45 largeShape
--cornerPointsWithDegreesSeq =  S.newCornerPointsWithDegreesBuilder 45 largeShape

--make a map of the largeShape with DegreeRange as key and CornerPoints as value
largeShapeMap  = M.fromList $ cornerPointsMap  largeShapeWithDegrees


{---------------------------------------- small shape ---------------------------
Create a smaller radial shape with same angles.
Change the origin so that it is moved inside of a single cube of large shape.-}
smallRadius = [(Radius x) | x <- [2,2..]]

smallShape =
  createBottomFaces (Point (5) (-10) 0) smallRadius angles flatXSlope flatYSlope
  |+++|
  createTopFaces (Point (5) (-10) 10) smallRadius angles flatXSlope flatYSlope

smallShapeWithDegrees = newCornerPointsWithDegreesList 45 smallShape

smallShapeMap = M.fromList $ cornerPointsMap smallShapeWithDegrees


  
{- ========================= the big one ============================
Build the large shape with the small shape removed from the 0-45 degree cube.
-}
buildHoleInSingleTriangle = do
  let 
      --access large shape cubes
      largeCube :: Double -> Double -> CornerPoints
      largeCube start end = getCubeBase largeShapeMap start end

      --access small shape(hole) cubes
      smallCube :: Double -> Double -> CornerPoints
      smallCube start end = getCubeBase smallShapeMap start end
      
      --keep large/smallCube DRY
      getCubeBase :: M.Map DegreeRange CornerPoints -> Double -> Double -> CornerPoints
      getCubeBase map start end = extractMaybeCube $ map^.at  (DegreeRange start end)

      frontFaceOfSmallCube start end =
        reverseNormal $ extractFrontFace $ smallCube start end
        
      
      f = -- 0-45 degrees of hole
          arr (\triangles ->
                    let backFace = getFrontFaceAsBackFace $ largeCube 0 45
                        
                    in   triangles S.>< (S.fromList (FacesBackBottomFrontTop +++^ (backFace +++ (frontFaceOfSmallCube 0 45))))
                  )

          -- 45-90 degrees of hole
          >>> arr (\triangles ->
                    let backFace = getFrontRightLineAsBackFace $ largeCube 45 90
                    in   triangles S.>< (S.fromList (FacesBottomFrontTop +++^ (backFace +++ (frontFaceOfSmallCube 45 90))))
                  )
          
          -- 90-135 degrees of hole
          >>> arr (\triangles ->
                    let largeCube' = getFrontRightLineAsBackFace $ largeCube 45 90
                    in   triangles S.>< (S.fromList (FacesBottomFrontTop +++^ (largeCube' +++ (frontFaceOfSmallCube 90 135))))
                  )
          -- 135-180 degrees of hole
          >>> arr (\triangles ->
                    let largeCube' = getRightFaceAsBackFace $ largeCube 45 90
                    in   triangles S.>< (S.fromList (FacesBottomFrontTop +++^ (largeCube' +++ (frontFaceOfSmallCube 135 180))))
                  )
          
          -- 180-225 degrees of hole
          >>> arr (\triangles ->
                    let largeCube' = getBackRightLineAsBackFace $ largeCube 45 90
                    in   triangles S.>< (S.fromList (FacesBottomFrontTop +++^ (largeCube' +++ (frontFaceOfSmallCube 180 225))))
                  )
          
          -- 225-270 degrees of hole
          >>> arr (\triangles ->
                    let largeCube' = getBackRightLineAsBackFace $ largeCube 45 90
                    in   triangles S.>< (S.fromList (FacesBottomFrontTop +++^ (largeCube' +++ (frontFaceOfSmallCube 225 270))))
                  )

          -- 270 315 degrees of hole
          >>> arr (\triangles ->
                    let largeCube' = reverseNormal $ getLeftFaceAsBackFace $ largeCube 315 360
                    in triangles S.>< (S.fromList(FacesBottomFrontTop +++^ (largeCube' +++ (frontFaceOfSmallCube 270 315))))

                  )
          
          -- 315-360 degrees of hole
          >>> arr (\triangles ->
                    let largeCube' = reverseNormal $ getFrontLeftLineAsBackFace $ largeCube 315 360
                    in triangles S.>< (S.fromList(FacesBottomFrontTop +++^ (largeCube' +++ (frontFaceOfSmallCube 315 360))))

                  )
          
          -- show the rest of the large triangle
          
          >>> arr (\triangles -> (S.fromList(FacesBottomFrontTop +++^ (largeCube 45 90))) S.>< triangles)
          
         
          >>> arr (\triangles ->
                   (S.fromList
                    ( [largeShapeWithDegrees]
                      L.||@~+++^|| 
                      [[FacesWithRange FacesBottomFrontTop (DegreeRange 90 315)]]
                    )
                   )
                    S.>< triangles
                   )

          >>> arr (\triangles ->
                    let largeCube' = largeCube 315 360
                    in  triangles S.>< (S.fromList (FacesBottomFrontTop +++^ largeCube'))
                    
                  )
          
          
          --print stl
          >>> Kleisli (\triangles -> writeStlToFile $ newStlShape "hole in the wall" (F.toList triangles))
          
  runKleisli f (S.fromList [])
  
  
buildHoleInSingleTriangleTest = do
  let largeRightFaceTest = TestCase $ assertEqual
        "largeRightFaceTest"
        (RightFace {b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                    f3 = Point {x_axis = 14.14213562373095, y_axis = -14.142135623730951, z_axis = 10.0},
                    f4 = Point {x_axis = 14.14213562373095, y_axis = -14.142135623730951, z_axis = 0.0}})
        (extractRightFace $ extractMaybeCube $ largeShapeMap^.at  (DegreeRange 45 90))
        
  runTestTT largeRightFaceTest

  let largeRightFaceAsBackFaceTest = TestCase $ assertEqual
        "largeRightFaceAsBackFaceTest"
        
        (BackFace {b1 = Point {x_axis = 14.14213562373095, y_axis = -14.142135623730951, z_axis = 0.0},
                   b2 = Point {x_axis = 14.14213562373095, y_axis = -14.142135623730951, z_axis = 10.0},
                   b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                   b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}})
        (
          getRightFaceAsBackFace $ extractMaybeCube $ largeShapeMap^.at  (DegreeRange 45 90)
        )
  runTestTT largeRightFaceAsBackFaceTest

  let smallCubeFrontFaceTest = TestCase $ assertEqual
       "smallCubeFrontFaceTest"
       
       (FrontFace {f1 = Point {x_axis = 5.0, y_axis = -8.0, z_axis = 0.0},
                   f2 = Point {x_axis = 5.0, y_axis = -8.0, z_axis = 10.0},
                   f3 = Point {x_axis = 6.414213562373095, y_axis = -8.585786437626904, z_axis = 10.0},
                   f4 = Point {x_axis = 6.414213562373095, y_axis = -8.585786437626904, z_axis = 0.0}})
       (extractFrontFace $ extractMaybeCube $ smallShapeMap^.at  (DegreeRange 135 180))
  runTestTT smallCubeFrontFaceTest

  let smallCubeFrontFaceInvertedTest = TestCase $ assertEqual
       "smallCubeFrontFaceInvertedTest"
       (FrontFace {f1 = Point {x_axis = 6.414213562373095, y_axis = -8.585786437626904, z_axis = 0.0},
                   f2 = Point {x_axis = 6.414213562373095, y_axis = -8.585786437626904, z_axis = 10.0},
                   f3 = Point {x_axis = 5.0, y_axis = -8.0, z_axis = 10.0},
                   f4 = Point {x_axis = 5.0, y_axis = -8.0, z_axis = 0.0}})
       (reverseNormal $ extractFrontFace $ extractMaybeCube $ smallShapeMap^.at  (DegreeRange 135 180))
  runTestTT smallCubeFrontFaceInvertedTest


  let addTheFacesTest = TestCase $ assertEqual
       "addTheFacesTest"
       (CubePoints {f1 = Point {x_axis = 6.414213562373095, y_axis = -8.585786437626904, z_axis = 0.0},
                    f2 = Point {x_axis = 6.414213562373095, y_axis = -8.585786437626904, z_axis = 10.0},
                    f3 = Point {x_axis = 5.0, y_axis = -8.0, z_axis = 10.0},
                    f4 = Point {x_axis = 5.0, y_axis = -8.0, z_axis = 0.0},
                    b1 = Point {x_axis = 14.14213562373095, y_axis = -14.142135623730951, z_axis = 0.0},
                    b2 = Point {x_axis = 14.14213562373095, y_axis = -14.142135623730951, z_axis = 10.0},
                    b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                    b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}})
       (let
           invertedFrontFace = reverseNormal $ extractFrontFace $ extractMaybeCube $ smallShapeMap^.at  (DegreeRange 135 180)
           largeTriangle = extractMaybeCube $ largeShapeMap^.at  (DegreeRange 45 90)
           rightFaceAsBackFace =  getRightFaceAsBackFace $ largeTriangle
        in  rightFaceAsBackFace +++ invertedFrontFace
       )
  runTestTT addTheFacesTest

writeNFGCubeFromTest = do
  writeStlToFile $ newStlShape "hole in the wall"
    (FaceBottom +++^

    (CubePoints {f1 = Point {x_axis = 6.414213562373095, y_axis = -8.585786437626904, z_axis = 0.0},
                    f2 = Point {x_axis = 6.414213562373095, y_axis = -8.585786437626904, z_axis = 10.0},
                    f3 = Point {x_axis = 5.0, y_axis = -8.0, z_axis = 10.0},
                    f4 = Point {x_axis = 5.0, y_axis = -8.0, z_axis = 0.0},
                    b1 = Point {x_axis = 14.14213562373095, y_axis = -14.142135623730951, z_axis = 0.0},
                    b2 = Point {x_axis = 14.14213562373095, y_axis = -14.142135623730951, z_axis = 10.0},
                    b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                    b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}})


    )
