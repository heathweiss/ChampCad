{-# LANGUAGE TemplateHaskell #-} 
module Examples.ShoeLift.CrazyBBoots(generateCutTreadBottomStl, showCutTreadBottomCumulativeCornerPoints) where


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
                  

makeLenses ''TreadData


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








cutTreadBottom :: ExceptT BuilderError (State CpointsStack ) CpointsList
cutTreadBottom = do
  lvl1CutTreadBtmLeftLines <- buildCubePointsListWithAdd "lvl1X1CutTreadData"
                            ((B1 (head $ treadData^.lvl1X1CutTread)) +++> (map (F1) (tail $ treadData^.lvl1X1CutTread )))
                            idList

  lvl1CutTreadRightLines <- buildCubePointsListWithAdd "lvl1X2CutTreadData"
                            ((B4 (head $ treadData^.lvl1X2CutTread)) +++> (map (F4) (tail $ treadData^.lvl1X2CutTread )))
                            idList

  lvl1CutTreadBtmFaces <- buildCubePointsListWithAdd "bottomOfCutTreadFaceData"
                              lvl1CutTreadBtmLeftLines
                              lvl1CutTreadRightLines

  lvl2CutTreadTopLeftLines <- buildCubePointsListWithAdd "lvl2CutTreadTopLeftLines"
                              ((B2 (head $ treadData^.lvl2X1CutTread)) +++> (map (F2) (tail $ treadData^.lvl2X1CutTread )))
                              idList

  lvl2CutTreadTopRightLines <- buildCubePointsListWithAdd "lvl2CutTreadTopRightLines"
                              ((B3 (head $ treadData^.lvl2X2CutTread)) +++> (map (F3) (tail $ treadData^.lvl2X2CutTread )))
                              idList

  
  lvl2CutTreadTopFaces <- buildCubePointsListWithAdd "lvl2CutTreadTopFaces"
                          lvl2CutTreadTopLeftLines
                          lvl2CutTreadTopRightLines
                          

  lvl1And2CutTreadCubes <- buildCubePointsListWithAdd "lvl1And2CutTreadCubes"
                           lvl1CutTreadBtmFaces
                           lvl2CutTreadTopFaces

  --temporarily transposeZ to have a look at them. Can replace this once the second cut tread line is scanned.
  {-
  bottomOfCutTreadCubes <- buildCubePointsListWithAdd "tempTopForBottomOfCutTreadX1Data"
                                      ( map ((transposeZ (+100)). upperFaceFromLowerFace) lvl1CutTreadBtmFaces )
                                      lvl1CutTreadBtmFaces
  -}                                  
                            
  return lvl1And2CutTreadCubes


generateCutTreadBottomStl :: CpointsStack -> IO () 
generateCutTreadBottomStl  inState =
  let cpoints =  ((execState $ runExceptT cutTreadBottom) inState)
  in  writeStlToFile $ newStlShape "crazyB boots lift"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)


showCutTreadBottomCumulativeCornerPoints ::  CpointsStack -> IO ()
showCutTreadBottomCumulativeCornerPoints     inState =
  print $ show  ((evalState $ runExceptT (cutTreadBottom ) ) inState)
