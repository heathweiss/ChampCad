{-# LANGUAGE ParallelListComp #-}
module Examples.ShoeLift.SandalsAFO where

import CornerPoints.CornerPoints (CornerPoints(..), (+++>))
import CornerPoints.Points(Point(..))
import CornerPoints.FaceConversions(upperFaceFromLowerFace, backFaceFromFrontFace )
import CornerPoints.FaceExtraction(extractFrontFace, extractTopFace, extractBottomFace, extractRightFace)
import CornerPoints.Transpose(transposeX, transposeY, transposeZ)
import CornerPoints.MeshGeneration(autoGenerateEachCube, autoGenerateEachFace)
import CornerPoints.Rotations(rotateAlongYaxis90, rotateAlongYaxis180)
import CornerPoints.Slicer(slice)

import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)

import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsList,
                     CpointsStack, CpointsList)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader


{--------------------------- overview-------------------------
Built June 2016
AFO sandals

Use 3D capture built from drafting board.

Use cubical input system.

Use ExceptT State builder

Use Slicr module to cut and rotate, instead of doing with Slicr3r

The new 3D capture system gave both sides as X cood's instead of 1 X cood and a width.
This does not need to get converted to a width system so it works with Cubical, as I can directly use CornerPoints
Maybe Cubical should have both systems?

------------------------ printing notes:----------------------
-----heel btm slice:
5 % honeycomb infill
-honeycombs are about 25 mm across.
-should try 4 %
1 perimeter
no detect thin walls
no extra perimeters if needed
Used Slicr3r to rotate 180 degrees on y axis

Arch was short as I did not include the last input from heel.
There has to be an overlap of inputs.
Add it in and print it again.

------heel top slice
-same as heel btm slice

-----arch bottom
-same as heel btm

-----arch top
Done.
X width at the slice did not work out well.

-----toe bottom
-}



--curry in the stack pushing function
buildCubePointsList' = buildCubePointsList (++)

{-
The heel of the sandal.
Will slice it off and rotate for flat printing.
-}
----------------------------------------------- HeelSandalBtmSlice----------------------------------------
generateHeelSandalBtmSliceStl =
  let faces = generateHeelSandalBtmSliceFaces []
      triangles' = [FacesAll | x <- [1..]] |+++^| faces
  in
  writeStlToFile $ newStlShape "HeelSandalBtmSlice" triangles'

generateHeelSandalBtmSliceFaces inState =
  autoGenerateEachCube [] ((execState $ runExceptT heelSandalBtmSlice ) inState)

{-
Take the bottom slice of the heel.
-}
heelSandalBtmSlice :: ExceptT BuilderError (State CpointsStack ) CpointsList
heelSandalBtmSlice = do
  topFaces <- buildCubePointsList' "create top faces" ((head heelSandal) +++> (tail heelSandal)) [CornerPointsId | x <- [1..]]
  btmFaces <- buildCubePointsList' "create top faces" ((head treadHeel) +++> (tail treadHeel)) [CornerPointsId | x <- [1..]]
  slicedTops <- buildCubePointsList' "create sliced tops"
                  [ slice 110 topFace btmFace
                      | topFace <- topFaces
                      | btmFace <- btmFaces
                  ]
                  [CornerPointsId | x <- [1..]]
  btmSlices <- buildCubePointsList' "create bottom slice" slicedTops btmFaces
  return  btmSlices

-------------------------------- HeelSandalTopSlice --------------------------------------------
reportGenerateHeelSandalTopSliceFaces :: IO ()
reportGenerateHeelSandalTopSliceFaces = do
  print $ show $ (evalState $ runExceptT heelSandalTopSlice )  []

generateHeelSandalTopSliceStl =
  let faces = generateHeelSandalTopSliceFaces []
      triangles' = [FacesAll | x <- [1..]] |+++^| faces
  in
  writeStlToFile $ newStlShape "HeelSandalBtmSlice" triangles'

generateHeelSandalTopSliceFaces inState =
  autoGenerateEachCube [] ((execState $ runExceptT heelSandalTopSlice ) inState)
  
{-Take the top slice of the heel-}
heelSandalTopSlice :: ExceptT BuilderError (State CpointsStack ) CpointsList
heelSandalTopSlice = do
  topFaces <- buildCubePointsList' "create top faces" ((head heelSandal) +++> (tail heelSandal)) [CornerPointsId | x <- [1..]]
  btmFaces <- buildCubePointsList' "create bottom faces" ((head treadHeel) +++> (tail treadHeel)) [CornerPointsId | x <- [1..]]
  slicedBottoms <- buildCubePointsList' "create sliced bottom"
                  [ slice 110 btmFace topFace
                      | topFace <- topFaces
                      | btmFace <- btmFaces
                  ]
                  [CornerPointsId | x <- [1..]]
  topSlices <- buildCubePointsList' "create bottom slice" slicedBottoms topFaces
  return  topSlices  


-------------------------------------------------- arch bottom slice ---------------------
generateArchBtmSliceStl =
  let faces = generateArchBtmSliceFaces []
      triangles' = [FacesAll | x <- [1..]] |+++^| faces
  in
  writeStlToFile $ newStlShape "HeelSandalBtmSlice" triangles'

generateArchBtmSliceFaces inState =
  autoGenerateEachCube [] ((execState $ runExceptT archBtmSlice ) inState)
{-
Take the bottom slice of the heel.
-}
archBtmSlice :: ExceptT BuilderError (State CpointsStack ) CpointsList
archBtmSlice = do
  topFaces <- buildCubePointsList' "create top faces" ((head sandalArch) +++> (tail sandalArch)) [CornerPointsId | x <- [1..]]
  btmFaces <- buildCubePointsList' "create top faces" ((head treadArch) +++> (tail treadArch)) [CornerPointsId | x <- [1..]]
  slicedTops <- buildCubePointsList' "create sliced tops"
                  [ slice 90 topFace btmFace
                      | topFace <- topFaces
                      | btmFace <- btmFaces
                  ]
                  [CornerPointsId | x <- [1..]]
  btmSlices <- buildCubePointsList' "create bottom slice" slicedTops btmFaces
  return  btmSlices

----------------------------arch top slice--------------------------------------
reportGenerateArchTopSliceFaces :: IO ()
reportGenerateArchTopSliceFaces = do
  print $ show $ (evalState $ runExceptT archTopSlice )  []

generateArchTopSliceStl =
  let faces = generateArchTopSliceFaces []
      triangles' = [FacesAll | x <- [1..]] |+++^| faces
  in
  writeStlToFile $ newStlShape "HeelSandalBtmSlice" triangles'

generateArchTopSliceFaces inState =
  autoGenerateEachCube [] ((execState $ runExceptT archTopSlice ) inState)
  
{-Take the bottom slice of the arch-}
archTopSlice :: ExceptT BuilderError (State CpointsStack ) CpointsList
archTopSlice = do
  topFaces <- buildCubePointsList' "create top faces" ((head sandalArch) +++> (tail sandalArch)) [CornerPointsId | x <- [1..]]
  btmFaces <- buildCubePointsList' "create bottom faces" ((head treadArch) +++> (tail treadArch)) [CornerPointsId | x <- [1..]]
  slicedBottoms <- buildCubePointsList' "create sliced bottom"
                  [ slice 90 btmFace topFace
                      | topFace <- topFaces
                      | btmFace <- btmFaces
                  ]
                  [CornerPointsId | x <- [1..]]
  topSlices <- buildCubePointsList' "create bottom slice" slicedBottoms topFaces
  return  topSlices  



------------------------------------------- toe btm slice -----------------------------
reportGenerateToeBottomSliceFaces :: IO ()
reportGenerateToeBottomSliceFaces = do
  print $ show $ (evalState $ runExceptT toeBtmSlice )  []

generateToeBtmSliceStl =
  let faces = generateToeBtmSliceFaces []
      triangles' = [FacesAll | x <- [1..]] |+++^| faces
  in
  writeStlToFile $ newStlShape "HeelSandalBtmSlice" triangles'

generateToeBtmSliceFaces inState =
  autoGenerateEachCube [] ((execState $ runExceptT toeBtmSlice ) inState)
{-
Take the bottom slice of the heel.
-}
toeBtmSlice :: ExceptT BuilderError (State CpointsStack ) CpointsList
toeBtmSlice = do
  topFaces <- buildCubePointsList' "create top faces" ((head sandalToe) +++> (tail sandalToe)) [CornerPointsId | x <- [1..]]
  btmFaces <- buildCubePointsList' "create top faces" ((head treadToe) +++> (tail treadToe)) [CornerPointsId | x <- [1..]]
  slicedTops <- buildCubePointsList' "create sliced tops"
                  [ slice 75 topFace btmFace
                      | topFace <- topFaces
                      | btmFace <- btmFaces
                  ]
                  [CornerPointsId | x <- [1..]]
  btmSlices <- buildCubePointsList' "create bottom slice" slicedTops btmFaces
  return  btmSlices

-------------------------- toe top slice ---------------------------------
reportGenerateToeTopSliceFaces :: IO ()
reportGenerateToeTopSliceFaces = do
  print $ show $ (evalState $ runExceptT toeTopSlice )  []

generateToeTopSliceStl =
  let faces = generateToeTopSliceFaces []
      triangles' = [FacesAll | x <- [1..]] |+++^| faces
  in
  writeStlToFile $ newStlShape "HeelSandalBtmSlice" triangles'

generateToeTopSliceFaces inState =
  autoGenerateEachCube [] ((execState $ runExceptT toeTopSlice ) inState)
  
{-Take the bottom slice of the arch-}
toeTopSlice :: ExceptT BuilderError (State CpointsStack ) CpointsList
toeTopSlice = do
  topFaces <- buildCubePointsList' "create top faces" ((head sandalToe) +++> (tail sandalToe)) [CornerPointsId | x <- [1..]]
  btmFaces <- buildCubePointsList' "create bottom faces" ((head treadToe) +++> (tail treadToe)) [CornerPointsId | x <- [1..]]
  slicedBottoms <- buildCubePointsList' "create sliced bottom"
                  [ slice 75 btmFace topFace
                      | topFace <- topFaces
                      | btmFace <- btmFaces
                  ]
                  [CornerPointsId | x <- [1..]]
  topSlices <- buildCubePointsList' "create bottom slice" slicedBottoms topFaces
  return  topSlices  



heelSandal :: [CornerPoints]
heelSandal =
  [ BackTopLine (Point  40   99.8  133.7)   (Point 50   99.8  133.7),
    FrontTopLine (Point 40   99.9  133.7)   (Point 50   99.9  133.7),
    FrontTopLine (Point 40   100   133.7)   (Point 50   100  133.7),
    FrontTopLine (Point 35   102   133.5)   (Point 60   102   133.5),
    FrontTopLine (Point 32   105   133.1)   (Point 65.3 105   133.1),
    FrontTopLine (Point 24   110   133)     (Point 71   110   133), 
    FrontTopLine (Point 21.2 115   131.9)   (Point 76   115   131.9),
    FrontTopLine (Point 16.5 120   130.8)   (Point 77.7 120   130.8),
    FrontTopLine (Point 15.3 125   129.7)   (Point 80   125   129.7),
    FrontTopLine (Point 15.3 130   128.3)   (Point 83   130   128.3),
    FrontTopLine (Point 13.5 140   125.8)   (Point 84   140   125.8),
    FrontTopLine (Point 13   150   123.7)   (Point 85   150   123.7),
    FrontTopLine (Point 11.5 160   122.4)   (Point 85   160   122.4),
    FrontTopLine (Point 11   170   119.7)   (Point 86   170   119.7),
    FrontTopLine  (Point 11   180   117.2)   (Point 87   180   117.2)
  ]

treadHeel :: [CornerPoints]
treadHeel =
  [
    BackBottomLine  (Point 44   93  102.3) (Point 48   93  102.3),
    BottomFrontLine (Point 35   95  102.3) (Point 65   95  102.3),
    BottomFrontLine (Point 24   100 102.3) (Point 70.5 100 102.3),
    BottomFrontLine (Point 23   102 102.4) (Point 72   102 102.4),
    BottomFrontLine (Point 20.5 105 101.6) (Point 73.5 105 101.6),
    BottomFrontLine (Point 19.5 110 101)   (Point 77   110 101), 
    BottomFrontLine (Point 16   115 100.9) (Point 78.5 115 100),--orig 100.9
    BottomFrontLine (Point 16   120 98.6)  (Point 81.5 120 98.6),
    BottomFrontLine (Point 15   125 97.5)  (Point 81.5 125 97.5),
    BottomFrontLine (Point 15   130 96.9)  (Point 82.5 130 96.9),
    BottomFrontLine (Point 15   140 94.1)  (Point 82.5 140 94.1),
    BottomFrontLine (Point 15   150 91.9)  (Point 82.5 150 91.9),
    BottomFrontLine (Point 16   160 86.8)  (Point 82.5 160 86.8),
    BottomFrontLine (Point 16   170 84.8)  (Point 82.5 170 84.8),
    BottomFrontLine  (Point 17   180   79)   (Point 81   180  79)
  ]

sandalArch :: [CornerPoints]
sandalArch =
  [
    BackTopLine  (Point 11   180   117.2)   (Point 87   180   117.2),
    FrontTopLine (Point 11   190   115.8)   (Point 87.5 190   115.8),
    FrontTopLine (Point 10.5 200   114.7)   (Point 87   200   114.7),
    FrontTopLine (Point 8.5  210   111.7)   (Point 86.5 210   111.7),
    FrontTopLine (Point 7    220   109.9)   (Point 89   220   109.9),
    FrontTopLine (Point 3.5  230   107.7)   (Point 90   230   107.7),
    FrontTopLine (Point 2.5  240   104.8)   (Point 91   240   104.8),
    FrontTopLine (Point 2.5  250   103.4)   (Point 91   250   103.4),
    FrontTopLine (Point 1    260   101.4)   (Point 90.5 260   101.4)
  ]


treadArch :: [CornerPoints]
treadArch =
  [
    BackBottomLine  (Point 17   180   79)   (Point 81   180  79),
    BottomFrontLine (Point 18   190   73.9) (Point 81   190  73.9),
    BottomFrontLine (Point 17   200   68.6) (Point 81   200  68.6),
    BottomFrontLine (Point 15.5 210   60.6) (Point 81.5 210  60.6),
    BottomFrontLine (Point 13.5 220   54)   (Point 83.3 220  54),
    BottomFrontLine (Point 10   230   49.2) (Point 85   230  49.2),
    BottomFrontLine (Point 8.5  240   45.9) (Point 86   240  45.9),
    BottomFrontLine (Point 7.5  250   44.1) (Point 89   250  44.1),
    BottomFrontLine (Point 7    260   44)   (Point 90   260  44)
  ]

sandalToe :: [CornerPoints]
sandalToe =
  [ BackTopLine  (Point 1    260   101.4)   (Point 90.5 260   101.4),
    FrontTopLine (Point 2.5  270   100)     (Point 90.5 270   100),
    FrontTopLine (Point 2.5  280   99.6)    (Point 91   280   99.6),
    FrontTopLine (Point 1  290   98.3)      (Point 89   290   98.3),
    FrontTopLine (Point 1.8  300   98.1)    (Point 83.5 300   98.1),
    FrontTopLine (Point 3.5  305   97.3)    (Point 79.5 305   97.3),
    FrontTopLine (Point 3.5 310   97.3)     (Point 78.5 310   97.3),
    FrontTopLine (Point 5   315   97.3)     (Point 77.5 315   97.3),
    FrontTopLine (Point 8   320   97.2)     (Point 73.5 320   97.2),
    FrontTopLine (Point 14 325   97.3)      (Point 68.5 325   97.3),
    FrontTopLine (Point 19   330   96.9)    (Point 63.5 330   96.9),
    FrontTopLine (Point 28 333   96.5)      (Point 60   333   96.5),
    FrontTopLine (Point 31 335   95.7)      (Point 56   335   95.7),--test: +100
    FrontTopLine (Point 34   338   95.7)    (Point 41   338   95.7),
    FrontTopLine (Point 37   338.1 95.7)    (Point 41   338.1 95.7), 
    FrontTopLine (Point 40   338.2 95.7)    (Point 41   338.2 95.7)
  ]



treadToe :: [CornerPoints]
treadToe =
  [
    BackBottomLine  (Point 7    260   44)   (Point 90   260  44),
    BottomFrontLine (Point 5    270   44.9) (Point 90   270 44.9),
    BottomFrontLine (Point 4    280   45.3) (Point 90   280 45.3),
    BottomFrontLine (Point 3.5  290   46.6) (Point 88.8 290 46.6),
    BottomFrontLine (Point 4    300   48.8) (Point 87.5 300 48.8),
    BottomFrontLine (Point 5    305   48.9) (Point 85   305 48.9),
    BottomFrontLine (Point 5    310   50.1) (Point 83   310 50.1),
    BottomFrontLine (Point 6    315   51.4) (Point 81.5 315 51.4),
    BottomFrontLine (Point 8.5  320   52.9) (Point 79   320 52.9),
    BottomFrontLine (Point 11   325   53.8) (Point 77.5 325 53.8),
    BottomFrontLine (Point 14   330   54)   (Point 73.5 330 54),
    BottomFrontLine (Point 17   333   54.5) (Point 72   333 54.5),
    BottomFrontLine (Point 18   335   54.5) (Point 70   335 54.5),
    BottomFrontLine (Point 23.5 338   55.1) (Point 63   338 55.1),
    BottomFrontLine (Point 28   340   55.5) (Point 58   340 55.5), 
    BottomFrontLine (Point 39   342   55.5) (Point 52   342 55.5)
  ]
