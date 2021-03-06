{-# LANGUAGE TemplateHaskell #-}

module Examples.ShoeLift.GeorgeSandalls(generateLiftMeetsShoeCubesToCxForErrors, 
                                        generateRearLiftMeetsShoeStl, generateForwardLiftMeetsShoeStl,
                                        generateLiftMeetsVibramCubesToCxForErrors, generateAllLiftMeetsVibramStl,
                                        generateRearLiftMeetsVibramStl, generateForwardLiftMeetsVibramStl,
                                        generateRearRiserSectionStl, generateRiserSectionCubesToCxForErrors
                                        ) where

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++>))
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace, extractFrontTopLine, extractBackTopLine)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace )
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)
import CornerPoints.MeshGeneration(autoGenerateEachCube)


import Stl.StlCornerPoints((|+++^|), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)

import Control.Lens

import Control.Monad.State.Lazy
import Control.Monad.Except

import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsList,
                     CpointsStack, CpointsList)

buildCubePointsListWithAdd  = buildCubePointsList (++)
idList = [CornerPointsId | x <-[1..]]


data TreadData =
  TreadData { --right side of sandall measurements
              _treadAsMeasuredX1 :: [Point],
              --left side of sandall measurements
              _treadAsMeasuredX2 :: [Point],
              --set of heights I hand created to create the shape of shoe where vibram tread is glued on.
              _vibramHeight :: [Double],
              --stick the vibram heights into a function that will work with transposeZ
              _vibramHeightAdj :: [(Double -> Double)],
              --give the left side of sandall, the vibram heights
              _vibramPointsX1 :: [Point],
              --give the right side of sandall, the vibram heights
              _vibramPointsX2 :: [Point]
                }

makeLenses ''TreadData

treadData =  TreadData
  {_treadAsMeasuredX1 =
    [
       Point (0.0) 0 69,
       Point (0.15) 0.7 68,
       Point (-6.55) 1.7 67.5,
       Point (-8.05) 3.2 67.5,
       Point (-11.45) 4.7 67,
       Point (-15.85) 6.7 66,
       Point (-19.75) 9.2 66,
       Point (-22.85) 13.2 64,
       Point (-27.95) 18.7 64,
       Point (-28.55) 24.7 62,
       Point (-30.55) 32.2 62,
       Point (-33.35) 39.7 62,
       Point (-34.15) 49.7 62,
       Point (-31.75) 59.7 62,
       Point (-29.85) 69.7 62,
       Point (-28.85) 79.7 64,
       Point (-24.45) 89.7 66,
       Point (-20.95) 99.7 67,
       Point (-18.05) 109.7 64,
       Point (-22.15) 119.7 63,
       Point (-26.15) 129.7 60,
       Point (-29.35) 139.7 60,
       Point (-36.05) 149.7 60,
       Point (-39.55) 159.7 61,
       Point (-42.75) 169.7 61.5,
       Point (-44.85) 179.7 61.5,
       Point (-44.55) 189.7 64.5,
       Point (-45.55) 199.7 67.5,
       Point (-42.55) 209.7 72,
       Point (-38.35) 219.7 77,
       Point (-33.45) 229.7 82,
       Point (-24.45) 239.7 89,
       Point (-20.95) 240.7 91.5,
       Point (-17.95) 241.7 92.5,--(-23.95) 241.7 92.5
       Point (-13.95) 242 92.5, --manually added
       Point (-3.95) 243 92.5 --manually added
    ],
   _treadAsMeasuredX2 =
    [
       Point (2.9) 0 69,
       Point (10.6) 0.7 68.5,
       Point (14.4) 1.7 68,
       Point (17.4) 3.2 67.5,
       Point (19.9) 4.7 66,
       Point (26.3) 6.7 66,
       Point (28.6) 9.2 66,
       Point (33.7) 13.2 64,
       Point (36.5) 18.7 64,
       Point (37.8) 24.7 62,
       Point (42.5) 32.2 62,
       Point (44) 39.7 62,
       Point (42.6) 49.7 61,
       Point (42.5) 59.7 61,
       Point (41.1) 69.7 62,
       Point (38.9) 79.7 62,
       Point (33.6) 89.7 63,
       Point (31.8) 99.7 65,
       Point (37.8) 109.7 65,
       Point (38.3) 119.7 63,
       Point (39.9) 129.7 60,
       Point (45) 139.7 60,
       Point (47.6) 149.7 60,
       Point (48.6) 159.7 61,
       Point (52.3) 169.7 61.5,
       Point (52.5) 179.7 64.5,
       Point (49) 189.7 66,
       Point (46.8) 199.7 67.5,
       Point (41.6) 209.7 72,
       Point (35.7) 219.7 77,
       Point (29.9) 229.7 82.5,
       Point (17.3) 239.7 89,
       Point (16.4) 240.7 90,
       Point (14.2) 241.7 91,
       Point (4.2) 242 91, --manually added
       Point (0.2) 243 91 --manually added
    ],
   --The heights required for the shape of the lift where the vibram tread is attached to the bottom.
   _vibramHeight =
    [
       31,
       29,
       27,
       22,
       19,
       15,
       12.5,
       8.5,
       4,
       1,
       0,
       0,
       0,
       0,
       0,
       2,
       3,
       5,
       6,
       3.5,
       1,
       0,
       0,
       1,
       2,
       3,
       6,
       10,
       17,
       25,
       35,
       51,
       52,
       53,
       54, 
       55 
    ],
   --turn the heights into a [(Double -> Double)] so they can be used to transpose the X1 X2 Points
   --The orignal z values will be ignored, with only the vibram heights used.
   _vibramHeightAdj = map ((\z_vibram origXheights -> (z_vibram) )) (treadData^.vibramHeight),
   --transpose the height of the X1 points to match the vibram heights
   _vibramPointsX1 = zipWith (transposeZ) (treadData^.vibramHeightAdj) (treadData^.treadAsMeasuredX1),
   --transpose the height of the X2 points to match the vibram heights
   _vibramPointsX2 = zipWith (transposeZ) (treadData^.vibramHeightAdj) (treadData^.treadAsMeasuredX2)
     
  }

-- =================================================== lift meets sandall ===========================================================
liftMeetsShoe :: ([Point] -> [Point]) -> ExceptT BuilderError (State CpointsStack ) CpointsList
liftMeetsShoe verticalSplitter = do
  measuredTopLeftLines <- buildCubePointsListWithAdd "measuredTopLeftLines"
                      ((B2 (head $ verticalSplitter $  treadData^.treadAsMeasuredX1)) +++> (map (F2) (tail $ verticalSplitter $ (treadData^.treadAsMeasuredX1) )))
                      idList
  
  measuredTopRightLines <- buildCubePointsListWithAdd "measuredTopRightLines"
                           ((B3 (head $ verticalSplitter $  treadData^.treadAsMeasuredX2)) +++> (map (F3) (tail $ verticalSplitter $ (treadData^.treadAsMeasuredX2) )))
                           idList

  measuredTopFaces <- buildCubePointsListWithAdd "measuredTopFaces"
                      measuredTopLeftLines
                      measuredTopRightLines


  flatBottomFaces <- buildCubePointsListWithAdd "flatBottomFaces"
                 (map ((transposeZ (\z -> (58))) . lowerFaceFromUpperFace) measuredTopFaces) --58 was the original test print which gave about 2 mill thinest section
                 idList

  cubes <- buildCubePointsListWithAdd "cubes"
           measuredTopFaces
           flatBottomFaces

  return cubes

generateLiftMeetsShoeCubesToCxForErrors :: IO ()
generateLiftMeetsShoeCubesToCxForErrors  =
  let initialState = []
  in  print $ show  ((evalState $ runExceptT (liftMeetsShoe (id)  ) ) initialState)


generateAllLiftMeetsShoeStl :: IO () 
generateAllLiftMeetsShoeStl =
  let initialState = []
  in  generateLiftMeetsShoeStlBase (id) initialState

generateRearLiftMeetsShoeStl :: IO () 
generateRearLiftMeetsShoeStl  =
  generateLiftMeetsShoeStlBase (take 22) []

generateForwardLiftMeetsShoeStl :: IO () 
generateForwardLiftMeetsShoeStl  =
  let initialState = []
  in generateLiftMeetsShoeStlBase (drop 21) initialState

generateLiftMeetsShoeStlBase :: ([Point] -> [Point]) -> CpointsStack -> IO ()
generateLiftMeetsShoeStlBase verticalSplitter inState = 
  let cpoints =  ((execState $ runExceptT (liftMeetsShoe (verticalSplitter))) inState)
  in  writeStlToFile $ newStlShape "george sandalls"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

-- ===================================== lift meets vibram sole ========================================================

liftMeetsVibram :: ([Point] -> [Point]) -> Double -> ExceptT BuilderError (State CpointsStack ) CpointsList
liftMeetsVibram verticalSplitter topHeight = do
  measuredTransformedBtmLeftLines <-
    buildCubePointsListWithAdd "measuredTransformedBtmLeftLines"
    ((B1 (head $ verticalSplitter $  treadData^.vibramPointsX1)) +++> (map (F1) (tail $ verticalSplitter $ (treadData^.vibramPointsX1) )))
    idList
  
  measuredTransformedBtmRightLines <-
    buildCubePointsListWithAdd "measuredTransformedBtmRightLines"
    ((B4 (head $ verticalSplitter $  treadData^.vibramPointsX2)) +++> (map (F4) (tail $ verticalSplitter $ (treadData^.vibramPointsX2) )))
    idList

  btmFaces <- buildCubePointsListWithAdd "btmFaces"
              measuredTransformedBtmLeftLines
              measuredTransformedBtmRightLines

  topFaces <- buildCubePointsListWithAdd "topFaces"
              (map ((transposeZ (\z -> topHeight)) . upperFaceFromLowerFace ) btmFaces)
              idList

  cubes <- buildCubePointsListWithAdd "cubes"
           btmFaces
           topFaces

  return cubes


generateLiftMeetsVibramCubesToCxForErrors :: IO ()
generateLiftMeetsVibramCubesToCxForErrors  =
  let initialState = []
      showFullLength = id
  in  print $ show  ((evalState $ runExceptT (liftMeetsVibram (showFullLength) 58  ) ) initialState)

generateLiftMeetsVibramStlBase :: ([Point] -> [Point]) -> Double -> CpointsStack -> IO ()
generateLiftMeetsVibramStlBase verticalSplitter topHeight inState = 
  let cpoints =  ((execState $ runExceptT (liftMeetsVibram (verticalSplitter) topHeight )) inState)
  in  writeStlToFile $ newStlShape "george sandalls"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

generateAllLiftMeetsVibramStl :: IO () 
generateAllLiftMeetsVibramStl =
  let initialState = []
  in  generateLiftMeetsVibramStlBase (id) 58 initialState


generateRearLiftMeetsVibramStl :: IO () 
generateRearLiftMeetsVibramStl =
  let initialState = []
  in  generateLiftMeetsVibramStlBase (take 22) 32 initialState

generateForwardLiftMeetsVibramStl :: IO () 
generateForwardLiftMeetsVibramStl =
  let initialState = []
  in  generateLiftMeetsVibramStlBase (drop 21) 58 initialState

-- ================================================= riser section ===================================================
riserSection :: ([Point] -> [Point]) -> Double -> Double -> ExceptT BuilderError (State CpointsStack ) CpointsList
riserSection verticalSplitter btmHeight topHeight = do
  measuredTransformedBtmLeftLines <-
    buildCubePointsListWithAdd "measuredTransformedBtmLeftLines"
    ((B1 (head $ verticalSplitter $  treadData^.vibramPointsX1)) +++> (map (F1) (tail $ verticalSplitter $ (treadData^.vibramPointsX1) )))
    idList
  
  measuredTransformedBtmRightLines <-
    buildCubePointsListWithAdd "measuredTransformedBtmRightLines"
    ((B4 (head $ verticalSplitter $  treadData^.vibramPointsX2)) +++> (map (F4) (tail $ verticalSplitter $ (treadData^.vibramPointsX2) )))
    idList

  btmFaces <- buildCubePointsListWithAdd "btmFaces"
              measuredTransformedBtmLeftLines
              measuredTransformedBtmRightLines

  btmFacesFlattened <- buildCubePointsListWithAdd "btmFacesFlattened"
                       (map (transposeZ (\origHeight -> btmHeight)) btmFaces)
                       idList

  topFaces <- buildCubePointsListWithAdd "topFaces"
              (map ((transposeZ (\z -> topHeight)) . upperFaceFromLowerFace ) btmFacesFlattened)
              idList

  cubes <- buildCubePointsListWithAdd "cubes"
           btmFacesFlattened
           topFaces

  return cubes


generateRiserSectionStlBase :: ([Point] -> [Point]) -> Double -> Double -> CpointsStack -> IO ()
generateRiserSectionStlBase verticalSplitter btmHeight topHeight inState = 
  let cpoints =  ((execState $ runExceptT (riserSection (verticalSplitter) btmHeight topHeight )) inState)
  in  writeStlToFile $ newStlShape "george sandalls"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

generateRearRiserSectionStl :: IO () 
generateRearRiserSectionStl =
  let initialState = []
  in  generateRiserSectionStlBase (take 22) 32 58 initialState

generateRiserSectionCubesToCxForErrors :: IO ()
generateRiserSectionCubesToCxForErrors  =
  let initialState = []
      showFullLength = id
  in  print $ show  ((evalState $ runExceptT (riserSection (showFullLength) 0 58  ) ) initialState)
