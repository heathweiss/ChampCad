{-# LANGUAGE TemplateHaskell #-} 
module Examples.ShoeLift.CrazyBBoots(generateHammerHeadSharkHeadSectionStl, showHammerHeadSharkHeadSectionCumulativeCornerPoints) where


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
                      CutTread {  _bottomOfCutTreadX1 :: [Point],
                                  _bottomOfCutTreadX2 :: [Point]
                               }
                  

makeLenses ''TreadData


buildCubePointsListWithAdd  = buildCubePointsList (++)
idList = [CornerPointsId | x <-[1..]]

treadData =
  CutTread
    { _bottomOfCutTreadX1 =
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
       Point (-24.5)   258 105.5,--(-26)   258 105.5
       Point (-21.5) 260 107,
       Point (-19)   261 109,
       Point (-16.5) 262 111,
       Point (-13)   263 112,
       Point (-6)    264 113

       
      ],
      _bottomOfCutTreadX2 =
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

      ]
     
    }








testShape :: ExceptT BuilderError (State CpointsStack ) CpointsList
testShape = do
  bottomOfCutTreadX1Data <- buildCubePointsListWithAdd "bottomOfCutTreadX1Data"
                            ((B1 (head $ treadData^.bottomOfCutTreadX1)) +++> (map (F1) (tail $ treadData^.bottomOfCutTreadX1 )))
                            idList

  bottomOfCutTreadX2Data <- buildCubePointsListWithAdd "bottomOfCutTreadX2Data"
                            ((B4 (head $ treadData^.bottomOfCutTreadX2)) +++> (map (F4) (tail $ treadData^.bottomOfCutTreadX2 )))
                            idList

  bottomOfCutTreadFaceData <- buildCubePointsListWithAdd "bottomOfCutTreadFaceData"
                              bottomOfCutTreadX1Data
                              bottomOfCutTreadX2Data

  tempTopForBottomOfCutTreadX1Data <- buildCubePointsListWithAdd "tempTopForBottomOfCutTreadX1Data"
                                      ( map ((transposeZ (+100)). upperFaceFromLowerFace) bottomOfCutTreadFaceData )
                                      bottomOfCutTreadFaceData
                                      
                            
  return tempTopForBottomOfCutTreadX1Data


generateHammerHeadSharkHeadSectionStl :: CpointsStack -> IO () 
generateHammerHeadSharkHeadSectionStl  inState =
  let cpoints =  ((execState $ runExceptT testShape) inState)
  in  writeStlToFile $ newStlShape "socket with quick release"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)


showHammerHeadSharkHeadSectionCumulativeCornerPoints ::  CpointsStack -> IO ()
showHammerHeadSharkHeadSectionCumulativeCornerPoints     inState =
  print $ show  ((evalState $ runExceptT (testShape ) ) inState)
