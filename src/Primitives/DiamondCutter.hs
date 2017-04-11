module Primitives.DiamondCutter(DiamondBuilder(..), runDiamondBuilder, runFrontToBackDiamondBuilder, OffSet(..),
                               isTheFrontDiamondDone, isTheBackDiamondDone, defaultDiamondBuilder) where
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|), (+++>))
import CornerPoints.Create(Origin(..), createCornerPoint)
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace, extractFrontTopLine, extractBackBottomLine,
                                    extractBackTopLine, extractRightFace, extractFrontRightLine, extractFrontLeftLine, extractBottomFrontLine,
                                    extractF2, extractF3, extractF4, extractF1, extractB1, extractB2, extractB3, extractB4, extractBackRightLine,
                                    extractBackLeftLine)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace, 
                                    f34LineFromF12Line, toBackFace, reverseNormal, toBottomFrontLine, toFrontTopLine,
                                    toFrontLeftLine, toFrontRightLine, toBackBottomLine, toBackTopLine, toBottomFace, toBackRightLine)


import Data.Maybe(isNothing, fromJust, isJust)

--ToDo: Figure out why flexSocket always hava an error that has to be fixed with Netfabb.

data DiamondBuilder =
  Diamond
    {outerCube :: CornerPoints,
     
     topDiamondFace :: Maybe CornerPoints,
     topDiamondCorner :: Maybe CornerPoints,
     topCenterPoint :: Maybe Point,
     topDiamondHorizontalOffsets :: OffSet,
     topDiamondVertiacalOffsets :: OffSet,
     
     topRightDiamondFace :: Maybe CornerPoints,

     rightDiamondFace :: Maybe CornerPoints,
     rightCenterPoint ::  Maybe Point,
     rightDiamondCorner :: Maybe CornerPoints,
     rightDiamondHorizontalOffsets :: OffSet,
     rightDiamondVerticalOffsets :: OffSet,
     
     bottomRightDiamondFace :: Maybe CornerPoints,
     
     bottomDiamondFace :: Maybe CornerPoints,
     bottomDiamondCorner :: Maybe CornerPoints,
     bottomCenterPoint :: Maybe Point,
     bottomDiamondHorizontalOffsets :: OffSet,
     bottomDiamondVerticalOffsets :: OffSet,

     bottomLeftDiamondFace :: Maybe CornerPoints,

     leftDiamondFace :: Maybe CornerPoints,
     leftCenterPoint ::  Maybe Point,
     leftDiamondCorner :: Maybe CornerPoints,
     leftDiamondHorizontalOffsets :: OffSet,
     leftDiamondVerticalOffsets :: OffSet,
     
     
     
     
     topLeftDiamondFace :: Maybe CornerPoints
     
     
    }


-- |
-- Run a single DiamondBuilder and output the resulting [diamond cubes].
-- If any of the cubes did not turn out, return a [CornerPointsError]
runDiamondBuilder :: DiamondBuilder -> [CornerPoints]
runDiamondBuilder diamondBuilder
  | isNothing $ topDiamondFace diamondBuilder = [CornerPointsError "topDiamondFace is Nothing "]
  | isNothing $ bottomDiamondFace diamondBuilder = [CornerPointsError "bottomDiamondFace is Nothing "]
  | isNothing $ rightDiamondFace diamondBuilder = [CornerPointsError "rightDiamondFace is Nothing "]
  | isNothing $ leftDiamondFace diamondBuilder = [CornerPointsError "leftDiamondFace is Nothing "]
  | isNothing $ topRightDiamondFace diamondBuilder = [CornerPointsError "topRightDiamondFace is Nothing "]
  | isNothing $ bottomRightDiamondFace diamondBuilder = [CornerPointsError "bottomRightDiamondFace is Nothing "]
  | isNothing $ bottomLeftDiamondFace diamondBuilder = [CornerPointsError "bottomLeftDiamondFace is Nothing "]
  | isNothing $ topLeftDiamondFace diamondBuilder = [CornerPointsError "topLeftDiamondFace is Nothing "] 
  | otherwise = [(fromJust $ topDiamondFace diamondBuilder),
                 (fromJust $ bottomDiamondFace diamondBuilder),
                 (fromJust $ rightDiamondFace diamondBuilder),
                 (fromJust $ leftDiamondFace diamondBuilder),
                 (fromJust $ topRightDiamondFace diamondBuilder),
                 (fromJust $ bottomRightDiamondFace diamondBuilder),
                 (fromJust $ bottomLeftDiamondFace diamondBuilder),
                 (fromJust $ topLeftDiamondFace diamondBuilder)]


-- |
-- Run the front/back DiamondBuilders, and add together the resulting [CornerPoints]
runFrontToBackDiamondBuilder :: (CornerPoints -> DiamondBuilder) -> CornerPoints -> [CornerPoints]
runFrontToBackDiamondBuilder diamondBuilder' cube =
  let diamondBuilder = diamondBuilder' cube
  in
  (runDiamondBuilder $ isTheFrontDiamondDone  diamondBuilder)
  |+++|
  (runDiamondBuilder $ isTheBackDiamondDone diamondBuilder)

data OffSet =
  OffSet {xOffset :: Double,
          yOffset :: Double,
          zOffset :: Double
         }


{-
A base used by Front and Back DiamondBuilders.
If all the cubes of a diamond are good, return them, else the error of the bad cube.
-}
isTheDiamondDoneBase :: (DiamondBuilder -> DiamondBuilder) -> (DiamondBuilder -> DiamondBuilder)
                     -> (DiamondBuilder -> DiamondBuilder) -> (DiamondBuilder -> DiamondBuilder)
                     -> (DiamondBuilder -> DiamondBuilder) -> (DiamondBuilder -> DiamondBuilder)
                     -> (DiamondBuilder -> DiamondBuilder) -> (DiamondBuilder -> DiamondBuilder)
                     -> DiamondBuilder -> DiamondBuilder
isTheDiamondDoneBase    topDiamondBuilder                     bottomDiamondBuilder
                        rightDiamondBuilder                   leftDiamondBuilder
                        topRightDiamondBuilder                bottomRightDiamondBuilder
                        bottomLeftDiamondBuilder              topLeftDiamondBuilder
                        diamondBuilder
  | (isNothing  $ topDiamondFace diamondBuilder) = topDiamondBuilder diamondBuilder
  | (isNothing  $ bottomDiamondFace diamondBuilder) = bottomDiamondBuilder diamondBuilder
  | (isNothing  $ rightDiamondFace diamondBuilder) = rightDiamondBuilder diamondBuilder
  | (isNothing  $ leftDiamondFace diamondBuilder) = leftDiamondBuilder diamondBuilder
  | (isNothing  $ topRightDiamondFace diamondBuilder) = topRightDiamondBuilder diamondBuilder
  | (isNothing  $ bottomRightDiamondFace diamondBuilder) = bottomRightDiamondBuilder diamondBuilder
  | (isNothing  $ bottomLeftDiamondFace diamondBuilder) = bottomLeftDiamondBuilder diamondBuilder
  | (isNothing  $ topLeftDiamondFace diamondBuilder) = topLeftDiamondBuilder diamondBuilder
  | otherwise  = diamondBuilder


-- |
-- Use isTheDiamondDoneBase for FrontFaces.
isTheFrontDiamondDone :: DiamondBuilder -> DiamondBuilder
isTheFrontDiamondDone diamondBuilder =
  isTheDiamondDoneBase (topDiamondFrontFaceBuilder) (bottomDiamondFrontFaceBuilder)
                       (frontRightDiamondBuilder) (frontLeftDiamondBuilder)
                       (frontTopRightDiamondBuilder) (frontBottomRightDiamondBuilder)
                       (frontBottomLeftDiamondBuilder) (frontTopLeftDiamondBuilder)
                       diamondBuilder

-- |
-- Use isTheDiamondDoneBase for BackFaces.
isTheBackDiamondDone :: DiamondBuilder -> DiamondBuilder
isTheBackDiamondDone diamondBuilder =
  isTheDiamondDoneBase (topDiamondBackFaceBuilder) (bottomDiamondBackFaceBuilder)
                       (backRightDiamondBuilder) (backLeftDiamondBuilder)
                       (backTopRightDiamondBuilder) (backBottomRightDiamondBuilder)
                       (backBottomLeftDiamondBuilder) (backTopLeftDiamondBuilder)
                       diamondBuilder

-- ==================================================== Top Diamond ==========================================================
{-
Base function used by Front and Back Faces.
Use to create the top cube of the diamond, that goes from the top point of the diamond, to the top face of the outer cube.

ToDo:
 All these main builder base functions could be combined into a single function.
 This would require the instances to have a bit more complicated functions ,via function composition, passed in.
-}
topDiamondFaceBuilderBase :: (CornerPoints -> Point) -> (CornerPoints -> Point) -> (Point -> CornerPoints) -> (CornerPoints -> CornerPoints)
                          -> (CornerPoints -> CornerPoints) -> (DiamondBuilder -> DiamondBuilder)
                          -> (DiamondBuilder -> DiamondBuilder) -> DiamondBuilder -> DiamondBuilder
topDiamondFaceBuilderBase point2Extractor point3Extractor topDiamondCornerConstructor extractTopLine toBottomLine bottomDiamondFaceBuilder isTheDiamondDone diamondBuilder =
  let topCenterPoint' =  case isNothing $ topCenterPoint diamondBuilder of
                           True -> 
                             {-
                             offsetPoint (xOffset $  topDiamondHorizontalOffsets diamondBuilder)
                                         (yOffset $ topDiamondHorizontalOffsets diamondBuilder)
                                         (zOffset $ topDiamondHorizontalOffsets diamondBuilder)
                                         (point2Extractor $ outerCube diamondBuilder) (point3Extractor $ outerCube diamondBuilder)
                             -}
                             offsetPoint' (topDiamondHorizontalOffsets diamondBuilder)
                                          (point2Extractor $ outerCube diamondBuilder)
                                          (point3Extractor $ outerCube diamondBuilder)
                           False -> fromJust $ topCenterPoint diamondBuilder
  in
  case (bottomCenterPoint diamondBuilder) of
    Nothing ->  bottomDiamondFaceBuilder $ diamondBuilder {topCenterPoint = Just topCenterPoint'}
    Just bottomCenterPoint' ->
      let topDiamondCorner' =  topDiamondCornerConstructor $ offsetPoint' (topDiamondVertiacalOffsets diamondBuilder)  topCenterPoint' bottomCenterPoint'
          topDiamondFace' = (extractTopLine $ outerCube diamondBuilder) +++ (toBottomLine $  topDiamondCorner')
      in  isTheDiamondDone $ diamondBuilder {topDiamondFace = Just topDiamondFace', topDiamondCorner = Just topDiamondCorner' , topCenterPoint = Just topCenterPoint'}
          
-- |
-- Use topDiamondFaceBuilderBase to build the FrontFace of the top cube.
topDiamondFrontFaceBuilder :: DiamondBuilder -> DiamondBuilder
topDiamondFrontFaceBuilder diamondBuilder =
  topDiamondFaceBuilderBase (f2) (f3) (F2) (extractFrontTopLine) (toBottomFrontLine) (bottomDiamondFrontFaceBuilder) (isTheFrontDiamondDone) diamondBuilder

-- |
-- Use topDiamondFaceBuilderBase to build the BackFace of the top cube.
topDiamondBackFaceBuilder :: DiamondBuilder -> DiamondBuilder
topDiamondBackFaceBuilder diamondBuilder =
  topDiamondFaceBuilderBase (b2) (b3) (B2) (extractBackTopLine) (toBackBottomLine) (bottomDiamondBackFaceBuilder) (isTheBackDiamondDone) diamondBuilder

-- =========================================================== Bottom Diamond =================================================================
{-
Base function used by Front and Back Faces.
Use to create the bottom cube of the diamond, that goes from the bottom point of the diamond, to the bottom face of the outer cube.
-}
bottomDiamondFaceBuilderBase :: (CornerPoints -> Point) -> (CornerPoints -> Point) -> (Point -> CornerPoints) -> (CornerPoints -> CornerPoints)
                             -> (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints)
                             -> (DiamondBuilder -> DiamondBuilder) -> DiamondBuilder -> DiamondBuilder
bottomDiamondFaceBuilderBase point1Extractor point4Extractor bottomDiamondCornerConstructor toTopLine extractBottomLine toBottomLine isTheDiamondDone diamondBuilder =
  let bottomCenterPoint' =
        case isNothing $ bottomCenterPoint diamondBuilder of
          True ->  offsetPoint' (bottomDiamondHorizontalOffsets diamondBuilder)
                                (point1Extractor $ outerCube diamondBuilder) (point4Extractor $ outerCube diamondBuilder)
          False -> fromJust $ bottomCenterPoint diamondBuilder
  in
  case topCenterPoint diamondBuilder of
    Nothing -> topDiamondFrontFaceBuilder $ diamondBuilder {bottomCenterPoint =  Just bottomCenterPoint'}
    Just topCenterPoint' ->
      let 
          bottomDiamondCorner' = bottomDiamondCornerConstructor $ offsetPoint' (bottomDiamondVerticalOffsets diamondBuilder)  bottomCenterPoint' topCenterPoint'
          bottomDiamondFace' =
            (reverseNormal $ toTopLine $ extractBottomLine $ outerCube diamondBuilder)
            +++
            (toBottomLine bottomDiamondCorner')
      in
      isTheDiamondDone $ diamondBuilder {bottomCenterPoint = Just bottomCenterPoint', bottomDiamondFace = Just bottomDiamondFace', bottomDiamondCorner = Just bottomDiamondCorner'}

-- |
-- Use bottomDiamondFaceBuilderBase to build the FrontFace of the bottom cube.
bottomDiamondFrontFaceBuilder :: DiamondBuilder -> DiamondBuilder
bottomDiamondFrontFaceBuilder diamondBuilder =
  bottomDiamondFaceBuilderBase (f1) (f4) (F4) (toFrontTopLine) (extractBottomFrontLine) toBottomFrontLine (isTheFrontDiamondDone) diamondBuilder

-- |
-- Use bottomDiamondFaceBuilderBase to build the BackFace of the bottom cube.
bottomDiamondBackFaceBuilder :: DiamondBuilder -> DiamondBuilder
bottomDiamondBackFaceBuilder diamondBuilder =
  bottomDiamondFaceBuilderBase (b1) (b4) (B4) (toBackTopLine) (extractBackBottomLine) (toBackBottomLine) (isTheBackDiamondDone) diamondBuilder

  -- ============================================================ Right Diamond =======================================================
{-A base function for creating the Front and Back faces of the right side of the outer cube.
Started as a copy of topDiamondFaceBuilderBase.
Re-written but not tested.

Needs: leftDiamondFaceBuilder
ToDo: build the front/back functions.
-}

rightDiamondFaceBuilderBase :: (CornerPoints -> Point) -> (CornerPoints -> Point) -> (Point -> CornerPoints) -> (CornerPoints -> CornerPoints)
                          -> (CornerPoints -> CornerPoints) -> (DiamondBuilder -> DiamondBuilder)
                          -> (DiamondBuilder -> DiamondBuilder) -> DiamondBuilder -> DiamondBuilder
rightDiamondFaceBuilderBase point3Extractor point4Extractor rightDiamondCornerConstructor extractTopLine toBottomLine leftDiamondFaceBuilder isTheDiamondDone diamondBuilder =
  let rightCenterPoint' =  case rightCenterPoint diamondBuilder of
                           Nothing -> 
                             
                             offsetPoint' (rightDiamondVerticalOffsets diamondBuilder)
                                         (point3Extractor $ outerCube diamondBuilder) (point4Extractor $ outerCube diamondBuilder)
                             {-
                             offsetPoint (xOffset $ rightDiamondVerticalOffsets diamondBuilder)
                                         (yOffset $ rightDiamondVerticalOffsets diamondBuilder)
                                         (zOffset $ rightDiamondVerticalOffsets diamondBuilder)
                                         (point3Extractor $ outerCube diamondBuilder) (point4Extractor $ outerCube diamondBuilder)
                             -}
                           Just rightCenterPoint'' -> rightCenterPoint''

  in
  case (leftCenterPoint diamondBuilder) of
    Nothing ->  leftDiamondFaceBuilder $ diamondBuilder {rightCenterPoint = Just rightCenterPoint'}
    Just leftCenterPoint' ->
      let rightDiamondCorner' =  rightDiamondCornerConstructor $ offsetPoint' (rightDiamondHorizontalOffsets diamondBuilder)  rightCenterPoint' leftCenterPoint'
          rightDiamondFace' = (extractTopLine $ outerCube diamondBuilder) +++ (toBottomLine $  rightDiamondCorner')
      in  isTheDiamondDone $ diamondBuilder {rightDiamondFace = Just rightDiamondFace', rightDiamondCorner = Just rightDiamondCorner', rightCenterPoint = Just rightCenterPoint'}
          

frontRightDiamondBuilder :: DiamondBuilder -> DiamondBuilder
frontRightDiamondBuilder diamondBuilder =
  rightDiamondFaceBuilderBase (f3) (f4) (F3) (toFrontTopLine . extractFrontRightLine) (toBottomFrontLine) (frontLeftDiamondBuilder) (isTheFrontDiamondDone) diamondBuilder

backRightDiamondBuilder :: DiamondBuilder -> DiamondBuilder
backRightDiamondBuilder diamondBuilder =
  rightDiamondFaceBuilderBase (b3) (b4) (B3) (toBackTopLine . extractBackRightLine) (toBackBottomLine) (backLeftDiamondBuilder) (isTheBackDiamondDone) diamondBuilder
-- ============================================================ Left Diamond =========================================================
{-A base function for creating the Front and Back faces of the left side of the outer cube.
Started as a copy of rightDiamondFaceBuilderBase.
not yet re-written but not tested.

Needs: leftDiamondFaceBuilder
ToDo: build the front/back functions.
-}
leftDiamondFaceBuilderBase :: (CornerPoints -> Point) -> (CornerPoints -> Point) -> (Point -> CornerPoints) -> (CornerPoints -> CornerPoints)
                          -> (CornerPoints -> CornerPoints) -> (DiamondBuilder -> DiamondBuilder)
                          -> (DiamondBuilder -> DiamondBuilder) -> DiamondBuilder -> DiamondBuilder
leftDiamondFaceBuilderBase topOuterPointExtractor btmOuterPointExtractor innerCPointConstructor extractOuterLine toInnerLine farDiamondBuilder isTheDiamondDone diamondBuilder =
  let myVerticallyOffsetPoint =  case leftCenterPoint diamondBuilder of
                           Nothing -> 
                             
                             offsetPoint' (leftDiamondVerticalOffsets diamondBuilder) (topOuterPointExtractor $ outerCube diamondBuilder)
                             (btmOuterPointExtractor $ outerCube diamondBuilder)
                             {-
                             offsetPoint (xOffset $ leftDiamondVerticalOffsets diamondBuilder)
                                         (yOffset $ leftDiamondVerticalOffsets diamondBuilder)
                                         (zOffset $ leftDiamondVerticalOffsets diamondBuilder)
                                         (topOuterPointExtractor $ outerCube diamondBuilder) (btmOuterPointExtractor $ outerCube diamondBuilder)
                             -}

                           Just myVerticallyOffsetPoint' -> myVerticallyOffsetPoint'

  
  in
  case (rightCenterPoint diamondBuilder) of
    Nothing ->  farDiamondBuilder $ diamondBuilder {leftCenterPoint = Just myVerticallyOffsetPoint}
    Just farVerticallyOffsetPoint ->
      let leftDiamondCorner' =  innerCPointConstructor $ offsetPoint' (leftDiamondHorizontalOffsets diamondBuilder) myVerticallyOffsetPoint farVerticallyOffsetPoint
          leftDiamondFace' = (extractOuterLine $ outerCube diamondBuilder) +++ (toInnerLine $  leftDiamondCorner')
      in  isTheDiamondDone $ diamondBuilder {leftDiamondFace = Just leftDiamondFace', leftDiamondCorner = Just leftDiamondCorner', leftCenterPoint = Just myVerticallyOffsetPoint}

frontLeftDiamondBuilder :: DiamondBuilder -> DiamondBuilder
frontLeftDiamondBuilder diamondBuilder =
  leftDiamondFaceBuilderBase (f2) (f1) (F1) (toFrontTopLine . extractFrontLeftLine) (toBottomFrontLine) (frontRightDiamondBuilder) (isTheFrontDiamondDone)  diamondBuilder

backLeftDiamondBuilder :: DiamondBuilder -> DiamondBuilder
backLeftDiamondBuilder diamondBuilder =
  leftDiamondFaceBuilderBase (b2) (b1) (B1) (toBackTopLine . extractBackLeftLine) (toBackBottomLine) (backRightDiamondBuilder) (isTheBackDiamondDone)  diamondBuilder

-- ================================================================= corner pieces =====================================================
{-
Try to make a single base that will handle all of the corner pieces.
-}
setMyFieldTopRight :: DiamondBuilder -> CornerPoints -> DiamondBuilder
setMyFieldTopRight  diamondBuilder cpoints =
  diamondBuilder {
  topRightDiamondFace = Just cpoints}

setMyFieldBottomRight :: DiamondBuilder -> CornerPoints -> DiamondBuilder
setMyFieldBottomRight  diamondBuilder cpoints =
  diamondBuilder {
  bottomRightDiamondFace = Just cpoints}

setMyFieldBottomLeft :: DiamondBuilder -> CornerPoints -> DiamondBuilder
setMyFieldBottomLeft  diamondBuilder cpoints =
  diamondBuilder {
  bottomLeftDiamondFace = Just cpoints}

setMyFieldTopLeft :: DiamondBuilder -> CornerPoints -> DiamondBuilder
setMyFieldTopLeft  diamondBuilder cpoints =
  diamondBuilder {
  topLeftDiamondFace = Just cpoints}

cornerPiecesDiamondBuilderBase :: (DiamondBuilder -> Maybe CornerPoints) -> (DiamondBuilder -> DiamondBuilder) 
                               -> (DiamondBuilder -> Maybe CornerPoints) -> (DiamondBuilder -> DiamondBuilder) -> (CornerPoints -> CornerPoints)
                               -> (DiamondBuilder -> CornerPoints -> DiamondBuilder)
                               -> (DiamondBuilder -> DiamondBuilder)  -> DiamondBuilder -> DiamondBuilder
cornerPiecesDiamondBuilderBase    previousDiamondExtractor                   previousDiamondBuilder               
                                  nextDiamondExtractor                       nextDiamondBuilder                    nextLineExtractor
                                  setMyField
                                  isTheDiamondDone diamondBuilder =
  case (previousDiamondExtractor diamondBuilder) of
    Nothing -> previousDiamondBuilder diamondBuilder
    Just previousDiamond ->
      case (nextDiamondExtractor diamondBuilder) of
        Nothing -> nextDiamondBuilder diamondBuilder
        Just nextDiamond ->
          isTheDiamondDone $ setMyField diamondBuilder
            (  previousDiamond
               +++
               (nextLineExtractor  nextDiamond)
            )
        

frontTopRightDiamondBuilder :: DiamondBuilder -> DiamondBuilder
frontTopRightDiamondBuilder diamondBuilder =
  cornerPiecesDiamondBuilderBase (topDiamondFace) (topDiamondFrontFaceBuilder) (rightDiamondFace) (frontRightDiamondBuilder)
  (toFrontRightLine . extractFrontLeftLine) (setMyFieldTopRight) isTheFrontDiamondDone diamondBuilder

frontBottomRightDiamondBuilder :: DiamondBuilder -> DiamondBuilder
frontBottomRightDiamondBuilder diamondBuilder =
  cornerPiecesDiamondBuilderBase (rightDiamondFace) (frontRightDiamondBuilder) (bottomDiamondFace) (bottomDiamondFrontFaceBuilder)
  (toFrontRightLine . extractFrontLeftLine) (setMyFieldBottomRight) isTheFrontDiamondDone diamondBuilder

frontBottomLeftDiamondBuilder :: DiamondBuilder -> DiamondBuilder
frontBottomLeftDiamondBuilder diamondBuilder =
  cornerPiecesDiamondBuilderBase (bottomDiamondFace) (bottomDiamondFrontFaceBuilder) (leftDiamondFace) (frontLeftDiamondBuilder)
  (toFrontRightLine . extractFrontLeftLine) (setMyFieldBottomLeft) isTheFrontDiamondDone diamondBuilder

frontTopLeftDiamondBuilder :: DiamondBuilder -> DiamondBuilder
frontTopLeftDiamondBuilder diamondBuilder =
  cornerPiecesDiamondBuilderBase (leftDiamondFace) (frontLeftDiamondBuilder) (topDiamondFace) (topDiamondFrontFaceBuilder)
  (toFrontRightLine . extractFrontLeftLine) (setMyFieldTopLeft) isTheFrontDiamondDone diamondBuilder

backTopRightDiamondBuilder :: DiamondBuilder -> DiamondBuilder
backTopRightDiamondBuilder diamondBuilder =
  cornerPiecesDiamondBuilderBase (topDiamondFace) (topDiamondBackFaceBuilder) (rightDiamondFace) (backRightDiamondBuilder)
  (toBackRightLine . extractBackLeftLine) (setMyFieldTopRight) isTheBackDiamondDone diamondBuilder

backBottomRightDiamondBuilder :: DiamondBuilder -> DiamondBuilder
backBottomRightDiamondBuilder diamondBuilder =
  cornerPiecesDiamondBuilderBase (rightDiamondFace) (backRightDiamondBuilder) (bottomDiamondFace) (bottomDiamondBackFaceBuilder)
  (toBackRightLine . extractBackLeftLine) (setMyFieldBottomRight) isTheBackDiamondDone diamondBuilder

backBottomLeftDiamondBuilder :: DiamondBuilder -> DiamondBuilder
backBottomLeftDiamondBuilder diamondBuilder =
  cornerPiecesDiamondBuilderBase (bottomDiamondFace) (bottomDiamondBackFaceBuilder) (leftDiamondFace) (backLeftDiamondBuilder)
  (toBackRightLine . extractBackLeftLine) (setMyFieldBottomLeft) isTheBackDiamondDone diamondBuilder


backTopLeftDiamondBuilder :: DiamondBuilder -> DiamondBuilder
backTopLeftDiamondBuilder diamondBuilder =
  cornerPiecesDiamondBuilderBase (leftDiamondFace) (backLeftDiamondBuilder) (topDiamondFace) (topDiamondFrontFaceBuilder)
  (toBackRightLine . extractBackLeftLine) (setMyFieldTopLeft) isTheBackDiamondDone diamondBuilder

-- ================================= offsets ====================================================
offsetPoint' :: OffSet -> Point ->         Point -> Point
offsetPoint'    offsets (Point x1 y1 z1) (Point x2 y2 z2) =
  let
     --origin = Point x1 y1 z1
     --pointAtFarEnd = Point x2 y2 z2
     --angleBetweenPoints = getXYAngle origin pointAtFarEnd 
     --distanceFromOrigin =  calcultateXYDistance origin pointAtFarEnd
     --shortenedDistanceFromOrigin =  Radius $  (radius distanceFromOrigin)  * (xOffset offsets)
     
  in
               Point (setAxis x1 x2 (xOffset offsets))
                     (setAxis y1 y2 (yOffset offsets))
                     (setAxis z1 z2 (zOffset offsets))

setAxis :: Double -> Double -> Double -> Double
setAxis    axis1        axis2        offset
      | axis1 == axis2 = axis1
      | axis1 < axis2  = axis1 + ((axis2 - axis1) * offset)
      | axis1 > axis2  = axis1 - ((axis1 - axis2) * offset)
{-                         
setX :: Double -> Double -> Double -> Double
setX    x1        x2        offset
      | x1 == x2 = x1
      | x1 < x2  = x1 + ((x2 - x1) * offset)
      | x1 > x2  = x1 - ((x1 - x2) * offset)


  

setZ :: Double -> Double -> Double -> Double
setZ    z1        z2        offset
     | z1 > z2 =  z1 - (((abs $ z1 - z2) * offset))
     | z1 < z2 =  z1 + (((abs $ z1 - z2) * offset))
     | otherwise = z1
-}
-- |
-- Create a defualt DiamondBuilder.
-- Offsets are all set at 25% in form edges of containing cube.
{- ================================================================================================================================
Use for testing. Tests good.
-}

defaultDiamondBuilder :: CornerPoints -> DiamondBuilder
defaultDiamondBuilder cube =
      Diamond { outerCube = cube,
                topDiamondFace = Nothing,
                topDiamondCorner = Nothing,
                topCenterPoint = Nothing,
                topDiamondHorizontalOffsets = (OffSet 0.5 0.5 0.5),
                topDiamondVertiacalOffsets = (OffSet 0.25 0.25 0.25),
                topRightDiamondFace = Nothing,
                rightDiamondFace = Nothing,
                rightCenterPoint = Nothing,
                rightDiamondCorner = Nothing,
                rightDiamondHorizontalOffsets = (OffSet 0.25 0.25 0.25),
                rightDiamondVerticalOffsets = (OffSet 0.5 0.5 0.5),
                bottomRightDiamondFace = Nothing,
                bottomDiamondFace = Nothing,
                bottomDiamondCorner = Nothing,
                bottomCenterPoint = Nothing,
                bottomDiamondHorizontalOffsets = (OffSet 0.5 0.5 0.5),
                bottomDiamondVerticalOffsets = (OffSet 0.25 0.25 0.25),
                bottomLeftDiamondFace = Nothing,

                leftDiamondFace = Nothing,
                leftCenterPoint = Nothing,
                leftDiamondCorner = Nothing,
                leftDiamondHorizontalOffsets = (OffSet 0.25 0.25 0.25),
                leftDiamondVerticalOffsets  = (OffSet 0.5 0.5 0.5),
                
                topLeftDiamondFace = Nothing
              }

