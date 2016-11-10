{-# LANGUAGE TemplateHaskell #-} 
module Examples.ShoeLift.CrazyBBoots(generateCutTreadFrontStlV2, generateCutTreadRearStlV2,
                                     generateBootTreadRearStl, generateBootTreadFrontStl, showBootTreadCubesState,
                                     generateCutTreadTestFitRearStl,
                                     generateBootLvl1RearStl, generateBootLvl1FrontStl) where


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

data TreadData =
                      CutTread {  _lvl1X1CutTread :: [Point],
                                  _lvl1X2CutTread :: [Point],
                                  _lvl1X1CutTreadYAdj :: [Point],
                                  _lvl1X2CutTreadYAdj :: [Point],
                                  _lvl2X1CutTread :: [Point],
                                  _lvl2X2CutTread :: [Point],
                                  _lvl1CutTreadAdjFxList :: [(Point -> Point)]
                               }
               |
                      BootTread
                               {  _lvl1X1BootTread :: [Point],
                                  _lvl1X2BootTread :: [Point],
                                  _lvl2X1BootTread :: [Point],
                                  _lvl2X2BootTread :: [Point]
                               }
               
                    
                  

makeLenses ''TreadData

--split the shoe lift as it is too long to print in 1 piece
rearVerticalSplit = 28
frontVerticalSplit = rearVerticalSplit - 1
--slice the top off of cutTread cubes so lift can be rotated 180 degrees for a flat print btm.
cutTreadHorizontalSlicerV1 = 140
cutTreadHorizontalSlicerV2 = 130
--cut the bottom of the boot tread cubes so it can have a flat btm to print.
--Leave a gap between cut/boot tread slicers so can print an adjustment piece.
bootTreadHorizontalSlicerV1 = cutTreadHorizontalSlicerV1 + 10
bootTreadHorizontalSlicerV2 = cutTreadHorizontalSlicerV2 + 10

buildCubePointsListWithAdd  = buildCubePointsList (++)
idList = [CornerPointsId | x <-[1..]]

{-
rear section of lvl1 is ~131 mm long, which is ~ 15mm too long.-}
treadData =
  CutTread
    { _lvl1X1CutTread =
      [Point (-3)  0 64,--(-0.5)  0 64
       Point (-7.5)  1  64,-- ?
       Point (-9.5)  2  64,--(-2)    2  64
       Point (-11.5) 3  64,--(-8)    3  64
       Point (-13.5) 4  64,--(-10)   4  64
       Point (-15)   5  64,--(-11)   5  64
       Point (-16.5) 6  64,--(-13.5) 6  64
       Point (-18.5) 7  64,--(-16.5) 7  64
       Point (-20)   8  64,--(-19.5) 8  64
       Point (-21.5) 9  64,--(-22)   9  64
       Point (-22.5) 10 64,
       Point (-24.5) 12 64,
       Point (-29)   15 64,
       Point (-31)   18 64,
       Point (-33.5) 21 64,--(-32.2) 21 64
       Point (-36)   25 64,
       Point (-37)   30 64,
       Point (-38)   35 64,
       Point (-38)   40 64,--(-37.5)
       Point (-38.5) 50 64,
       Point (-37)   60 64,
       Point (-36)   70 64,
       Point (-34.5) 80 64,
       Point (-32)   90 64,
       Point (-30.5) 100 64,
       Point (-29)   110 64,
       Point (-27)   120 64,
       Point (-26.5) 130 64,
       Point (-27.5) 140 61.5,
       Point (-30)   150 60,
       Point (-33)   160 60,
       Point (-36)   170 59,
       Point (-39)   180 59.5,
       Point (-39)   190 61.5,
       Point (-41)   200 64,--(-41)   200 64
       Point (-40.5) 205 66,--(-40.5) 205 65
       Point (-40)   210 69,
       Point (-40)   215 72,
       Point (-39)   220 74,
       Point (-39)   225 78,
       Point (-38)   230 80,
       Point (-37)   235 83,
       Point (-35.5) 240 87,
       Point (-34)   245 91,
       Point (-32)   250 95,--(-32)   250 98
       Point (-29.5) 253 99,
       Point (-27)   256 103,
       Point (-24.5) 258 105.5,--(-26)   258 105.5
       Point (-21.5) 260 107,
       Point (-19)   261 109,
       Point (-16.5) 262 111,
       Point (-13)   263 112,
       Point (-6)    264 113
      ],
      _lvl1X2CutTread =
      [Point 7  0  64,--0.5  0  64
       Point 11    1  64,-- ?
       Point 13   2  64,--4    2  64,
       Point 15   3  64,--10   3  64
       Point 16.5   4  64,--14.5 4  64
       Point 17.5   5  64,--16   5  64
       Point 19   6  64,
       Point 21.5 7  64,
       Point 23 8 64,
       Point 24 9 64,
       Point 25 10 64,
       Point 27 12 64,
       Point 29 15 64,
       Point 32 18 64,
       Point 34 21 64,
       Point 36 25 64,
       Point 37 30 64,
       Point 38 35 64,
       Point 39 40 64,
       Point 42 50 64,
       Point 43 60 64,
       Point 44 70 64,
       Point 45 80 64,
       Point 47 90 64,
       Point 47 100 64,
       Point 48.5 110 64,
       Point 49.5 120 61,
       Point 51.5 130 58,
       Point 53 140 57,--50 140 58
       Point 57 150 55,
       Point 59 160 55,
       Point 60 170 55,
       Point 60 180 56,
       Point 59 190 57,
       Point 57 200 59.5,--57 200 63
       Point 57 205 63,--57 205 65
       Point 56 210 65.5,--
       Point 56 215 68,
       Point 55 220 70.5,
       Point 53 225 74,
       Point 51 230 79,--51 230 78
       Point 51 235 83.5,--51 235 85
       Point 48 240 88,
       Point 45 245 92,
       Point 43 250 95.5,--43 250 96
       Point 40 253 98.5,
       Point 37 256 102,
       Point 34 258 104,--33 258 106
       Point 32 260 106,--32 260 106
       Point 29 261 108,
       Point 27 262 109.5,--26 262 109.5
       Point 25 263 111,
       Point 21 264 113	
      ],
      _lvl1CutTreadAdjFxList =
        let
          adjuster :: (Double -> Double) -> Point -> Point
          adjuster fx point =
             transposeY fx point
        in
          [adjuster (\y -> y + adjVal) | adjVal <- [15,14.4642857143..0]]  ++ [adjuster (\y -> y + adjVal) | adjVal <- [0,0..]],
      
      _lvl1X1CutTreadYAdj =
         (zipWith  ($) (treadData^.lvl1CutTreadAdjFxList) (treadData^.lvl1X1CutTread)),

      _lvl1X2CutTreadYAdj =
         (zipWith  ($) (treadData^.lvl1CutTreadAdjFxList) (treadData^.lvl1X2CutTread)),
          
       
      _lvl2X1CutTread =
      [Point (-3) (-1) 92,
       Point (-7.5) 0 92,
       Point (-9.5) 1 92,
       Point (-11.5) 2 92,
       Point (-13.5) 3 92,
       Point (-15) 4 92,
       Point (-16.5) 5 92,
       Point (-18.5) 6 92,
       Point (-20) 7 92,
       Point (-21.5) 8 92,
       Point (-22.5) 9 92,
       Point (-24.5) 11 92,
       Point (-29) 14 92,
       Point (-32) 17.5 91.5,
       Point (-35) 21 91,
       Point (-36) 25 90.5,
       Point (-37) 30 90,
       Point (-38) 35 89.5,
       Point (-38) 40 89.5,
       Point (-37) 50 88.5,
       Point (-36) 60 88.5,
       Point (-35) 70 88,
       Point (-33) 80 87,
       Point (-31) 90 86,
       Point  (-29) 100 85.5,
       Point (-27.5) 110 84,
       Point (-26) 120 83,
       Point (-27) 130 81.5,
       Point (-29) 140 80,
       Point (-33) 150 79.5,
       Point (-36) 160 79,
       Point (-39) 170 79,
       Point (-41) 180 81,
       Point (-41) 190 82,
       Point (-42) 200 86,
       Point (-40) 205 87,
       Point (-40) 210 91,
       Point (-39) 215 94,
       Point (-38.5) 220 97,
       Point (-38) 225 101,
       Point (-36.5) 230 104,
       Point (-35) 235 107.5,
       Point (-32.5) 240 112,
       Point (-30) 245 115,
       Point (-27) 250 119,
       Point (-24) 253 121,
       Point (-17.5) 256 125.5,
       Point (-13) 256.5 126.5,
       Point (-11) 257 127,
       Point (-9) 257.5 127,
       Point (-7) 258 127.5,
       Point (-6.5) 258.5 127.5,
       Point (-6) 259 127.5
      ],
      _lvl2X2CutTread =
      [Point 7 (-1) 92,
       Point 11 0 92,
       Point 13 1 92,
       Point 15 2 92,
       Point 16.5 3 92,
       Point 17.5 4 92,
       Point 19 5 92,
       Point 20.5 6 92,
       Point 23 7 92,
       Point 24 8 92,
       Point 25 9 92,
       Point 27 11 92,
       Point 29 14 92,
       Point 32 17.5 91.5,
       Point 36 21 91,
       Point 38 25 90.5,
       Point 40 30 89.5,
       Point 40.5 35 89,
       Point 41 40 88.5,
       Point 42 50 88,
       Point 44 60 87.5,
       Point 45 70 86,
       Point 46 80 84.5,
       Point 46 90 83 ,
       Point 47 100 82,
       Point 48 110 80.5,
       Point 50 120 79,
       Point 53 130 77,
       Point 55 140 76.5,
       Point 57.5 150 76,
       Point 59.5 160 76,
       Point 60 170 76,
       Point 61.5 180 78,
       Point 60 190 80,
       Point 59 200 85,
       Point 58 205 87,
       Point 57.5 210 89.5,
       Point 57 215 92.5,
       Point 56 220 95.5,
       Point 53.5 225 98.5,
       Point 51 230 102.5,
       Point 49 235 106,
       Point 47 240 110.5,
       Point 43 245 115,
       Point 37 250 119,
       Point 33 253 122,
       Point 26 256 124.5,
       Point 25.5 256.5 125,
       Point 25 257 125.5,
       Point 23 257.5 126,
       Point 18 258 127.5,
       Point 17.5 258.5 127.5,
       Point 17 259 127.5
      ]
    
     
    }


bootData =
  BootTread
    { _lvl1X1BootTread =
      [Point (-1) (-1) 192,
       Point (-3) 0 192,
       Point (-7.5) 1 192,
       Point (-11) 2 192,
       Point (-12.5) 3 191,
       Point (-16.5) 4 190,
       Point (-18) 5 189,
       Point (-19) 6 187.5,
       Point (-21) 7 186.5,
       Point (-22.5) 8 185,
       Point (-25) 9 184,
       Point (-26.5) 11 183,
       Point (-29) 14 182,
       Point (-31) 17.5 181.5,
       Point (-33.5) 21 180.75,
       Point (-35.5) 25 180,
       Point (-36) 30 179,
       Point (-36) 35 178,
       Point (-36) 40 178,
       Point (-36) 50 176,
       Point (-35) 60 176,
       Point (-34) 70 176,
       Point (-32) 80 176,
       Point (-27) 90 178,
       Point (-26) 100 180,
       Point (-24.5) 110 180,
       Point (-25.5) 120 177,
       Point (-28) 130 174,
       Point (-31) 140 171.5,
       Point (-32) 150 168,
       Point (-35) 160 168,
       Point (-35.5) 170 168,
       Point (-34.5) 180 168,
       Point (-33.5) 190 168,
       Point (-32) 200 168,
       Point (-31) 205 169,
       Point (-29) 210 170,
       Point (-26.5) 215 170,
       Point (-26) 220 172,
       Point (-23) 225 174,
       Point (-19) 230 175,
       Point (-15.5) 235 176,
       Point (-12.5) 240 179.5,
       Point (-7.5) 245 182,
       Point (-6) 246 182.5,
       Point (-5.5) 247 183,
       Point (-4.5) 248 183.5,
       Point (0) 249 185.5,
       Point (2) 250 186.5,
       Point (4) 250.5 187,
       Point (6) 251 187.5,
       Point (7) 251.5 188,
       Point (9) 252 188
      ],

      _lvl1X2BootTread =
      [Point (5) (-1) 192,
       Point (7) 0 191,
       Point (14) 1 190,
       Point (16) 2 188.5,
       Point (21) 3 187,
       Point (24) 4 186,
       Point (25) 5 185,
       Point (26) 6 184,
       Point (29) 7 183,
       Point (31) 8 181.5,
       Point (32) 9 181,
       Point (35) 11 180,
       Point (36) 14 178,
       Point (37.5) 17.5 177,
       Point (39.5) 21 175,
       Point (41) 25 174.5,
       Point (43) 30 173.5,
       Point (43) 35 171,
       Point (43) 40 171,
       Point (43) 50 170.5,
       Point (45) 60 169,
       Point (46) 70 169,
       Point (46.5) 80 171,
       Point (48) 90 173,
       Point (49) 100 173,
       Point (52) 110 170.5,
       Point (54) 120 168,
       Point (56) 130 165.5,
       Point (58) 140 163,
       Point (60) 150 161,
       Point (63) 160 159.5,
       Point (62) 170 158,
       Point (61.5) 180 158,
       Point (62.5) 190 160,
       Point (60.5) 200 162,
       Point (59.5) 205 163.5,
       Point (59) 210 164,
       Point (57) 215 165,
       Point (56) 220 167,
       Point (53.5) 225 168,
       Point (51) 230 168.5,
       Point (48) 235 171,
       Point (47) 240 174,
       Point (44) 245 179.5,
       Point (41) 246 182,
       Point (40) 247 183,
       Point (38) 248 183.5,
       Point (37) 249 184,
       Point (35) 250 185,
       Point (33) 250.5 186,
       Point (31) 251 187,
       Point (29) 251.5 188,
       Point (27) 252 188
      ],
      _lvl2X1BootTread =
      [Point (6) (-1) 213,
       Point (6.2) 0 213,
       Point (6.4) 1 213,
       Point (6.5) 2 213,
       Point (7) 3 213,
       Point (7.5) 4 213,
       Point (8) 5 213,
       Point (8.5) 6 213,
       Point (9) 7 213,
       Point (9.5) 8 213,
       Point (10) 9 213,
       Point (11) 11 213,
       Point (14) 14 213,
       Point (17.5) 17.5 213,
       Point (21) 21 213,
       Point (25) 25 213,
       Point (30) 30 213,
       Point (35) 35 212.5,
       Point (40) 40 211.5,
       Point (50) 50 210.5,
       Point (60) 60 208.5,
       Point (70) 70 205.5,
       Point (80) 80 205,
       Point (85) 90 205,
       Point (95) 100 202,
       Point (100) 110 202,
       Point (110) 120 202,
       Point (120) 130 203.5,
       Point (135) 140 195.5,
       Point (150) 150 192,
       Point (160) 160 192,
       Point (170) 170 190,
       Point (180) 180 190,
       Point (190) 190 190,
       Point (200) 200 190,
       Point (205) 205 190,
       Point (210) 210 189.5,
       Point (215) 215 189.5,
       Point (220) 220 189.5,
       Point (225) 225 189.5,
       Point (230) 230 189.5,
       Point (235) 235 189.5,
       Point (240) 240 187,
       Point (245) 245 187,
       Point (245.2) 246 187,
       Point (245.4) 247 187,
       Point (245.6) 248 187,
       Point (245.7) 249 187,
       Point (245.8) 250 187,
       Point (245.9) 250.5 187,
       Point (246) 251 187.5,
       Point (248) 251.5 198,
       Point (250) 252 198
      ],
      _lvl2X2BootTread =
      [Point (6) (-1) 213,
       Point (7) 0 213,
       Point (8) 1 213,
       Point (8.5) 2 213,
       Point (9) 3 213,
       Point (9.5) 4 213,
       Point (10) 5 213,
       Point (10.5) 6 213,
       Point (11) 7 213,
       Point (11.5) 8 213,
       Point (13) 9 213,
       Point (17) 11 213,
       Point (26) 14 213,
       Point (27) 17.5 211,
       Point (35) 21 209,
       Point (47) 25 208,
       Point (40) 30 207,
       Point (42) 35 206,
       Point (43) 40 206,
       Point (46) 50 205,
       Point (47) 60 203,
       Point (50) 70 201.5,
       Point (52) 80 200.5,
       Point (55) 90 200.5,
       Point (56.5) 100 205,
       Point (57) 110 206,
       Point (58) 120 201.5,
       Point (60) 130 195,
       Point (60) 140 194,
       Point (63) 150 187,
       Point (64) 160 185,
       Point (65) 170 185,
       Point (65) 180 185,
       Point (65) 190 185,
       Point (64) 200 185,
       Point (64) 205 185,
       Point (63) 210 185,
       Point (61) 215 185,
       Point (60) 220 185,
       Point (59) 225 185,
       Point (56) 230 185,
       Point (53) 235 185,
       Point (49) 240 185.5,
       Point (44) 245 186,
       Point (44) 246 186.2,
       Point (44) 247 186.4,
       Point (44) 248 186.6,
       Point (44) 249 186.7,
       Point (44) 250 186.8,
       Point (44) 250.5 186.9,
       Point (44) 251 187,
       Point (36) 251.5 197,
       Point (31.5) 252 198
      ]

    }
{-------------------------------------------------------------- BootTreadCubes--------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------
The upper section of the lift, which includes the shape of the boot tread, including downwards riser, still the shape of the lvl1 boot tread.
-}

{-
Print off just enough of the lvl 1 to have a flat btm, so it can be cx'd for fit.
-}
bootTreadLvl1Test :: ([Point] -> [Point]) -> ExceptT BuilderError (State CpointsStack ) CpointsList
bootTreadLvl1Test verticalSplitter = do
  lvl1BootTreadBtmLeftLines <- buildCubePointsListWithAdd "lvl1BootTreadBtmLeftLines"
                                   (lvl1BootTreadBtmLeftLinesBase verticalSplitter)
                                   idList

  lvl1BootTreadRightLines <- buildCubePointsListWithAdd "lvl1BootTreadRightLines"
                             (lvl1BootTreadBtmRightLinesBase verticalSplitter)
                             idList

  btmFaces <- buildCubePointsListWithAdd "lvl1BootTreadBtmFaces"
              lvl1BootTreadBtmLeftLines
              lvl1BootTreadRightLines

  topFaces <- buildCubePointsListWithAdd "topFaces"
              (map (upperFaceFromLowerFace) btmFaces)
              idList

  btmFacesFlat <- buildCubePointsListWithAdd "btmFacesFlat"
                  (map (transposeZ (\z -> 155)) btmFaces)
                  idList

  btmCubes     <- buildCubePointsListWithAdd "btmCubes"
                  btmFacesFlat
                  topFaces

  return btmCubes


generateBootLvl1TestStlBase :: ([Point] -> [Point]) -> CpointsStack -> IO () 
generateBootLvl1TestStlBase verticalSplitter  inState =
  let cpoints =  ((execState $ runExceptT (bootTreadLvl1Test verticalSplitter)) inState)
  in  writeStlToFile $ newStlShape "crazyB boots lift"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

generateBootLvl1RearStl :: IO ()
generateBootLvl1RearStl  =
  generateBootLvl1TestStlBase (take $ rearVerticalSplit) []

generateBootLvl1FrontStl :: IO ()
generateBootLvl1FrontStl  =
  generateBootLvl1TestStlBase (drop $ frontVerticalSplit) []

bootTreadCubes :: ([Point] -> [Point]) -> ExceptT BuilderError (State CpointsStack ) CpointsList
bootTreadCubes verticalSplitter = do
  lvl1BootTreadBtmLeftLines <- buildCubePointsListWithAdd "lvl1BootTreadBtmLeftLines"
                                   (lvl1BootTreadBtmLeftLinesBase verticalSplitter)
                                   idList

  lvl1BootTreadRightLines <- buildCubePointsListWithAdd "lvl1BootTreadRightLines"
                             (lvl1BootTreadBtmRightLinesBase verticalSplitter)
                             idList

  lvl1BootTreadBtmFaces <- buildCubePointsListWithAdd "lvl1BootTreadBtmFaces"
                           lvl1BootTreadBtmLeftLines
                           lvl1BootTreadRightLines

  riserCubes <- buildCubePointsListWithAdd "riserCubes"
                (map
                   (upperFaceFromLowerFace)
                   lvl1BootTreadBtmFaces
                )
                (map
                   (transposeZ (\z -> bootTreadHorizontalSlicerV1))
                   lvl1BootTreadBtmFaces
                )
                

  return riserCubes

generateBootTreadStlBase :: ([Point] -> [Point]) -> CpointsStack -> IO () 
generateBootTreadStlBase verticalSplitter  inState =
  let cpoints =  ((execState $ runExceptT (bootTreadCubes verticalSplitter)) inState)
  in  writeStlToFile $ newStlShape "crazyB boots lift"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

generateBootTreadRearStl :: IO ()
generateBootTreadRearStl  =
  generateBootTreadStlBase (take $ rearVerticalSplit) []

generateBootTreadFrontStl :: IO ()
generateBootTreadFrontStl  =
  generateBootTreadStlBase (drop $ frontVerticalSplit) []

showBootTreadCubesState ::  CpointsStack -> IO ()
showBootTreadCubesState     inState =
  print $ show  ((evalState $ runExceptT ((bootTreadCubes (id)) ) ) inState)
{-------------------------------------------------------------- cutTreadCubes--------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------
The lower section of the lift, which includes the shape of the cut tread, and a riser up to the shape of the lvl1 boot tread.

Gets slice off on the top, so that can rotate 180 degrees around y axis in Slicr3r, for a flat print btm.
-}


{--------V1
Version 1:
Too narrow to fit over the cut tread.
Putting the lift on top of the cut tread works better, especially considering it has a natural ledge to sit on.
This means:
-that the lvl2 is not needed.
-the lift will sit ~1-1.5 cm higher, so the height should be adusted accordingly.

Next:
see V2

Printer notes:
Used 3 perimeters. Was good, perhaps should try 2.
Used 5% honeycomb infill. Very strong, but still light. 4% might be adequate, but Slicr does not give that option.
Rotated 180 degrees around y axis to have flat print btm.
-}
cutTreadCubesV1 :: ([Point] -> [Point]) -> ExceptT BuilderError (State CpointsStack ) CpointsList
cutTreadCubesV1 verticalSplitter = do
  lvl1CutTreadBtmLeftLines <- buildCubePointsListWithAdd "lvl1X1CutTreadData"
                            (  (B1 (head $ verticalSplitter $ treadData^.lvl1X1CutTread))  +++> (map (F1) (tail $ verticalSplitter $ treadData^.lvl1X1CutTread )))
                            idList

  lvl1CutTreadRightLines <- buildCubePointsListWithAdd "lvl1X2CutTreadData"
                            ((B4 (head $ verticalSplitter $ treadData^.lvl1X2CutTread)) +++> (map (F4) (tail $ verticalSplitter $ treadData^.lvl1X2CutTread )))
                            idList

  lvl1CutTreadBtmFaces <- buildCubePointsListWithAdd "bottomOfCutTreadFaceData"
                              lvl1CutTreadBtmLeftLines
                              lvl1CutTreadRightLines

  lvl2CutTreadTopLeftLines <- buildCubePointsListWithAdd "lvl2CutTreadTopLeftLines"
                              ((B2 (head $ verticalSplitter $ treadData^.lvl2X1CutTread)) +++> (map (F2) (tail $ verticalSplitter $ treadData^.lvl2X1CutTread )))
                              idList

  lvl2CutTreadTopRightLines <- buildCubePointsListWithAdd "lvl2CutTreadTopRightLines"
                              ((B3 (head $ verticalSplitter $ treadData^.lvl2X2CutTread)) +++> (map (F3) (tail $ verticalSplitter $ treadData^.lvl2X2CutTread )))
                              idList

  
  lvl2CutTreadTopFaces <- buildCubePointsListWithAdd "lvl2CutTreadTopFaces"
                          lvl2CutTreadTopLeftLines
                          lvl2CutTreadTopRightLines
                          

  lvl1And2CutTreadCubes <- buildCubePointsListWithAdd "lvl1And2CutTreadCubes"
                           lvl1CutTreadBtmFaces
                           lvl2CutTreadTopFaces

  lvl1BootTreadBtmLeftLines <- buildCubePointsListWithAdd "lvl1BootTreadBtmLeftLines"
                               ((B1 (head $ verticalSplitter $ bootData^.lvl1X1BootTread)) +++> (map (F1) (tail $ verticalSplitter $ bootData^.lvl1X1BootTread )))
                               idList

  lvl1BootTreadRightLines <- buildCubePointsListWithAdd "lvl1BootTreadRightLines"
                             ((B4 (head $ verticalSplitter $ bootData^.lvl1X2BootTread)) +++> (map (F4) (tail $ verticalSplitter $ bootData^.lvl1X2BootTread )))
                             idList

  lvl1BootTreadBtmFaces <- buildCubePointsListWithAdd "lvl1BootTreadBtmFaces"
                          lvl1BootTreadBtmLeftLines
                          lvl1BootTreadRightLines

  cutTreadToBootTreadAdaptor <- buildCubePointsListWithAdd "cutTreadToBootTreadAdaptor"
                                (map ((transposeZ (\z -> cutTreadHorizontalSlicerV1))  . upperFaceFromLowerFace) lvl1BootTreadBtmFaces)
                                lvl1And2CutTreadCubes
                               
  return cutTreadToBootTreadAdaptor


generateCutTreadStlBaseV1 :: ([Point] -> [Point]) -> CpointsStack -> IO () 
generateCutTreadStlBaseV1 verticalSplitter  inState =
  let cpoints =  ((execState $ runExceptT (cutTreadCubesV1 verticalSplitter)) inState)
  in  writeStlToFile $ newStlShape "crazyB boots lift"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

generateCutTreadRearStlV1 :: IO ()
generateCutTreadRearStlV1  =
  generateCutTreadStlBaseV1 (take $ rearVerticalSplit) []

generateCutTreadFrontStlV1 :: IO ()
generateCutTreadFrontStlV1  =
  generateCutTreadStlBaseV1 (drop $ frontVerticalSplit) []

showCutTreadCubesStateV1 ::  CpointsStack -> IO ()
showCutTreadCubesStateV1     inState =
  print $ show  ((evalState $ runExceptT ((cutTreadCubesV1 (id)) ) ) inState)

-- -----===================V2

{-
Print a thin section to check the size.
Do 2 perims to see what that is like, as 3 perims does not fit over the tread.
No infill.
-}
cutTreadLvl1TestFit :: ([Point] -> [Point]) -> ExceptT BuilderError (State CpointsStack ) CpointsList
cutTreadLvl1TestFit verticalSplitter = do
  lvl1CutTreadBtmLeftLines <- buildCubePointsListWithAdd "lvl1X1CutTreadData"
                             (
                              (B1 (head $ verticalSplitter $ treadData^.lvl1X1CutTreadYAdj))
                              +++>
                              (map (F1) (tail $ verticalSplitter $ treadData^.lvl1X1CutTreadYAdj ))
                             )
                             idList

  lvl1CutTreadRightLines <- buildCubePointsListWithAdd "lvl1X2CutTreadData"
                            (
                              (B4 (head $ verticalSplitter $ treadData^.lvl1X2CutTreadYAdj))
                             +++>
                             (map (F4) (tail $ verticalSplitter $ treadData^.lvl1X2CutTreadYAdj ))
                            )
                            idList

  lvl1CutTreadBtmFaces <- buildCubePointsListWithAdd "bottomOfCutTreadFaceData"
                              lvl1CutTreadBtmLeftLines
                              lvl1CutTreadRightLines

  lvl1CutTreadCubes <- buildCubePointsListWithAdd "lvl1CutTreadTopFaces"
                          (map (upperFaceFromLowerFace .  (transposeZ (\z -> 68))) lvl1CutTreadBtmFaces)
                          lvl1CutTreadBtmFaces

  return lvl1CutTreadCubes


generateCutTreadTestFitStlBase :: ([Point] -> [Point]) -> CpointsStack -> IO () 
generateCutTreadTestFitStlBase verticalSplitter  inState =
  let cpoints =  ((execState $ runExceptT (cutTreadLvl1TestFit verticalSplitter)) inState)
  in  writeStlToFile $ newStlShape "crazyB boots lift"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

generateCutTreadTestFitRearStl :: IO ()
generateCutTreadTestFitRearStl  =
  generateCutTreadTestFitStlBase (take $ rearVerticalSplit) []

{-
Changes from V1:
-eliminate cut tread lvl 2
-lower boot tread lvl 1 too compensate for lift sitting higher on the cut tread.
-}
cutTreadCubesV2 :: ([Point] -> [Point]) -> ExceptT BuilderError (State CpointsStack ) CpointsList
cutTreadCubesV2 verticalSplitter = do
  lvl1CutTreadBtmLeftLines <- buildCubePointsListWithAdd "lvl1X1CutTreadData"
                            (  (B1 (head $ verticalSplitter $ treadData^.lvl1X1CutTreadYAdj))  +++> (map (F1) (tail $ verticalSplitter $ treadData^.lvl1X1CutTreadYAdj )))
                            idList

  lvl1CutTreadRightLines <- buildCubePointsListWithAdd "lvl1X2CutTreadData"
                            ((B4 (head $ verticalSplitter $ treadData^.lvl1X2CutTreadYAdj)) +++> (map (F4) (tail $ verticalSplitter $ treadData^.lvl1X2CutTreadYAdj )))
                            idList

  lvl1CutTreadBtmFaces <- buildCubePointsListWithAdd "bottomOfCutTreadFaceData"
                              lvl1CutTreadBtmLeftLines
                              lvl1CutTreadRightLines

  lvl1BootTreadBtmLeftLines <- buildCubePointsListWithAdd "lvl1BootTreadBtmLeftLines"
                               ((B1 (head $ verticalSplitter $ bootData^.lvl1X1BootTread)) +++> (map (F1) (tail $ verticalSplitter $ bootData^.lvl1X1BootTread )))
                               idList

  lvl1BootTreadRightLines <- buildCubePointsListWithAdd "lvl1BootTreadRightLines"
                             ((B4 (head $ verticalSplitter $ bootData^.lvl1X2BootTread)) +++> (map (F4) (tail $ verticalSplitter $ bootData^.lvl1X2BootTread )))
                             idList

  lvl1BootTreadBtmFaces <- buildCubePointsListWithAdd "lvl1BootTreadBtmFaces"
                          lvl1BootTreadBtmLeftLines
                          lvl1BootTreadRightLines

  cutTreadToBootTreadAdaptor <- buildCubePointsListWithAdd "cutTreadToBootTreadAdaptor"
                                (map ((transposeZ (\z -> cutTreadHorizontalSlicerV2))  . upperFaceFromLowerFace) lvl1BootTreadBtmFaces)
                                lvl1CutTreadBtmFaces

  return cutTreadToBootTreadAdaptor


generateCutTreadStlBaseV2 :: ([Point] -> [Point]) -> CpointsStack -> IO () 
generateCutTreadStlBaseV2 verticalSplitter  inState =
  let cpoints =  ((execState $ runExceptT (cutTreadCubesV2 verticalSplitter)) inState)
  in  writeStlToFile $ newStlShape "crazyB boots lift"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

generateCutTreadRearStlV2 :: IO ()
generateCutTreadRearStlV2  =
  generateCutTreadStlBaseV2 (take $ rearVerticalSplit) []

generateCutTreadFrontStlV2 :: IO ()
generateCutTreadFrontStlV2  =
  generateCutTreadStlBaseV2 (drop $ frontVerticalSplit) []

showCutTreadCubesStateV2 ::  CpointsStack -> IO ()
showCutTreadCubesStateV2     inState =
  print $ show  ((evalState $ runExceptT ((cutTreadCubesV2 (id)) ) ) inState)
{---------------------------------------------------------- common cube builders ---------------------------------------------------}
lvl1BootTreadBtmLeftLinesBase :: ([Point] -> [Point]) -> [CornerPoints]
lvl1BootTreadBtmLeftLinesBase verticalSplitter =
  ((B1 (head $ verticalSplitter $ bootData^.lvl1X1BootTread)) +++> (map (F1) (tail $ verticalSplitter $ bootData^.lvl1X1BootTread )))

lvl1BootTreadBtmRightLinesBase :: ([Point] -> [Point]) -> [CornerPoints]
lvl1BootTreadBtmRightLinesBase verticalSplitter =
  ((B4 (head $ verticalSplitter $ bootData^.lvl1X2BootTread)) +++> (map (F4) (tail $ verticalSplitter $ bootData^.lvl1X2BootTread )))
