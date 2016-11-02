{-# LANGUAGE TemplateHaskell #-} 
module Examples.ShoeLift.CrazyBBoots(generateRearStl, generateFrontStl, showLowerCubesState) where


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

--import Control.Monad.Trans.Except
--import Control.Monad.Trans.Maybe
--import Control.Monad
--import Control.Monad.Trans
import Control.Monad.State.Lazy
--import Control.Monad.State
import Control.Monad.Except
--import Control.Monad.Writer (WriterT, tell, execWriterT)
--import Control.Monad.Reader

import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsList,
                     CpointsStack, CpointsList)

data TreadData =
                      CutTread {  _lvl1X1CutTread :: [Point],
                                  _lvl1X2CutTread :: [Point],
                                  _lvl2X1CutTread :: [Point],
                                  _lvl2X2CutTread :: [Point]
                               }
               |
                      BootTread
                               {  _lvl1X1BootTread :: [Point],
                                  _lvl1X2BootTread :: [Point]
                                --  _lvl2X1CutTread :: [Point],
                                --  _lvl2X2CutTread :: [Point]
                               }
               
                    
                  

makeLenses ''TreadData

--split the shoe lift as it is too big to print in 1 piece
rearHorizontalSplit = 28
frontHorizontalSplit = rearHorizontalSplit - 1

buildCubePointsListWithAdd  = buildCubePointsList (++)
idList = [CornerPointsId | x <-[1..]]

treadData =
  CutTread
    { _lvl1X1CutTread =
      [Point (-3)  0 64,--(-0.5)  0 64
       Point (-7.5)  1  64,-- ?
       Point (-9.5)    2  64,--(-2)    2  64
       Point (-11.5)    3  64,--(-8)    3  64
       Point (-13.5)   4  64,--(-10)   4  64
       Point (-15)   5  64,--(-11)   5  64
       Point (-16.5) 6  64,--(-13.5) 6  64
       Point (-18.5) 7  64,--(-16.5) 7  64
       Point (-20) 8  64,--(-19.5) 8  64
       Point (-21.5)   9  64,--(-22)   9  64
       Point (-22.5) 10 64,
       Point (-24.5) 12 64,
       Point (-29)   15 64,
       Point (-31)   18 64,
       Point (-33.5) 21 64,--(-32.2) 21 64
       Point (-36)   25 64,
       Point (-37)   30 64,
       Point (-38)   35 64,
       Point (-38) 40 64,--(-37.5)
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
       Point (-12.5) 3 190,
       Point (-16.5) 4 190,
       Point (-18) 5 189,
       Point (-19) 6 187,
       Point (-21) 7 185,
       Point (-22.5) 8 185,
       Point (-25) 9 184,
       Point (-26.5) 11 183,
       Point (-29) 14 182,
       Point (-31) 17.5 180,
       Point (-33.5) 21 180,
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
       Point (-6) 246 183,
       Point (-5.5) 247 183,
       Point (-4.5) 248 183.5,
       Point (0) 249 186.5,
       Point (2) 250 187,
       Point (4) 250.5 187,
       Point (6) 251 187.5,
       Point (7) 251.5 188,
       Point (9) 252 188
      ],

      _lvl1X2BootTread =
      [Point (5) (-1) 190,
       Point (7) 0 190,
       Point (14) 1 192,
       Point (16) 2 188,
       Point (21) 3 187,
       Point (24) 4 186,
       Point (25) 5 183,
       Point (26) 6 183,
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
       Point (41) 246 183,
       Point (41) 247 183,
       Point (38) 248 183,
       Point (37) 249 184,
       Point (35) 250 185,
       Point (33) 250.5 186,
       Point (31) 251 187,
       Point (29) 251.5 188,
       Point (27) 252 188

      ]

    }





lowerCubes :: ([Point] -> [Point]) -> ExceptT BuilderError (State CpointsStack ) CpointsList
lowerCubes splitter = do
  lvl1CutTreadBtmLeftLines <- buildCubePointsListWithAdd "lvl1X1CutTreadData"
                            (  (B1 (head $ splitter $ treadData^.lvl1X1CutTread))  +++> (map (F1) (tail $ splitter $ treadData^.lvl1X1CutTread )))
                            idList

  lvl1CutTreadRightLines <- buildCubePointsListWithAdd "lvl1X2CutTreadData"
                            ((B4 (head $ splitter $ treadData^.lvl1X2CutTread)) +++> (map (F4) (tail $ splitter $ treadData^.lvl1X2CutTread )))
                            idList

  lvl1CutTreadBtmFaces <- buildCubePointsListWithAdd "bottomOfCutTreadFaceData"
                              lvl1CutTreadBtmLeftLines
                              lvl1CutTreadRightLines

  lvl2CutTreadTopLeftLines <- buildCubePointsListWithAdd "lvl2CutTreadTopLeftLines"
                              ((B2 (head $ splitter $ treadData^.lvl2X1CutTread)) +++> (map (F2) (tail $ splitter $ treadData^.lvl2X1CutTread )))
                              idList

  lvl2CutTreadTopRightLines <- buildCubePointsListWithAdd "lvl2CutTreadTopRightLines"
                              ((B3 (head $ splitter $ treadData^.lvl2X2CutTread)) +++> (map (F3) (tail $ splitter $ treadData^.lvl2X2CutTread )))
                              idList

  
  lvl2CutTreadTopFaces <- buildCubePointsListWithAdd "lvl2CutTreadTopFaces"
                          lvl2CutTreadTopLeftLines
                          lvl2CutTreadTopRightLines
                          

  lvl1And2CutTreadCubes <- buildCubePointsListWithAdd "lvl1And2CutTreadCubes"
                           lvl1CutTreadBtmFaces
                           lvl2CutTreadTopFaces

  lvl1BootTreadBtmLeftLines <- buildCubePointsListWithAdd "lvl1BootTreadBtmLeftLines"
                               ((B1 (head $ splitter $ bootData^.lvl1X1BootTread)) +++> (map (F1) (tail $ splitter $ bootData^.lvl1X1BootTread )))
                               idList

  lvl1BootTreadRightLines <- buildCubePointsListWithAdd "lvl1BootTreadRightLines"
                             ((B4 (head $ splitter $ bootData^.lvl1X2BootTread)) +++> (map (F4) (tail $ splitter $ bootData^.lvl1X2BootTread )))
                             idList

  lvl1BootTreadBtmFaces <- buildCubePointsListWithAdd "lvl1BootTreadBtmFaces"
                          lvl1BootTreadBtmLeftLines
                          lvl1BootTreadRightLines

  cutTreadToBootTreadAdaptor <- buildCubePointsListWithAdd "cutTreadToBootTreadAdaptor"
                                (map ((transposeZ (\z -> 140))  . upperFaceFromLowerFace) lvl1BootTreadBtmFaces)
                                lvl1And2CutTreadCubes
                               
  return cutTreadToBootTreadAdaptor


generateStlBase :: ([Point] -> [Point]) -> CpointsStack -> IO () 
generateStlBase splitter  inState =
  let cpoints =  ((execState $ runExceptT (lowerCubes splitter)) inState)
  in  writeStlToFile $ newStlShape "crazyB boots lift"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

generateRearStl :: IO ()
generateRearStl  =
  generateStlBase (take $ rearHorizontalSplit) []

generateFrontStl :: IO ()
generateFrontStl  =
  generateStlBase (drop $ frontHorizontalSplit) []

showLowerCubesState ::  CpointsStack -> IO ()
showLowerCubesState     inState =
  print $ show  ((evalState $ runExceptT ((lowerCubes (id)) ) ) inState)
