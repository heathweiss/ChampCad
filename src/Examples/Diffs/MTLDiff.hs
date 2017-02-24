
module Examples.Diffs.MTLDiff(generateSingleLargeCubeToCxForErrors, generateSingleLargeCubeStl,
                              generatecutterCubesToCxForErrors, generatecutterCubesStl,
                              generateUnionCubesToCxForErrors, generateUnionCubesStl) where

import Control.Monad.State.Lazy
import Control.Monad.Except



import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsList,
                     CpointsStack, CpointsList)

import CornerPoints.Radius(Radius(..))
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces)
import CornerPoints.Create(Angle(..), Origin(..))
import CornerPoints.CornerPoints((|@+++#@|), (|+++|),  CornerPoints(..), (+++), getCornerPointsWithIndex)
import CornerPoints.Points(Point(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)
import CornerPoints.FaceConversions(toBackFace, reverseNormal, toFrontFace, backFaceFromFrontFace, frontFaceFromBackFace,
                                    f12LineFromF34Line, f34LineFromF12Line, b12LineFromF12Line, b34LineFromF34Line,
                                    )
import CornerPoints.FaceExtraction(extractFrontFace, extractFrontLeftLine, extractFrontRightLine, extractLeftFace,
                                  extractRightFace, extractBackRightLine, extractBackLeftLine, extractBackFace, extractBackLeftLine)


import Stl.StlCornerPoints((|+++^|), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)

buildCubePointsListWithAdd  = buildCubePointsList (++)
buildCubePointsListWithNoAdd = buildCubePointsList (\newCubes cubeStack -> cubeStack)
idList = [CornerPointsId | x <-[1..]]


--angles for small/large shapes
angles = map Angle [0,45]

{------------------------------------- large cube ------------------------------
Create the large shape, which will have the small shape cut from it.

This will be a single slice of a radial shape.
-}
largeRadius = [(Radius x) | x <- [20,20..]]

singleLargeCube :: ExceptT BuilderError (State CpointsStack ) CpointsList
singleLargeCube = do
  btmFaces <-
    buildCubePointsListWithAdd "btmFaces"
    (createBottomFaces (Point 0 0 0) largeRadius angles )
    idList
   
  topFaces <-
    buildCubePointsListWithAdd "topFaces"
    (createTopFaces (Point 0 0 10) largeRadius angles )
    idList

  singleCube <-
    buildCubePointsListWithAdd "singleCube"
    btmFaces
    topFaces
    
  return btmFaces


generateSingleLargeCubeToCxForErrors :: IO ()
generateSingleLargeCubeToCxForErrors  =
  let initialState = []
      
  in  print $ show  (    (evalState $ runExceptT singleLargeCube)    initialState)

generateSingleLargeCubeStl :: CpointsStack -> IO ()
generateSingleLargeCubeStl inState = 
  let cpoints =  ((execState $ runExceptT (singleLargeCube )) inState)
  in  writeStlToFile $ newStlShape "single large cube"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)


----------------------------------- small radial shape used as cutter --------------------------------------
{-
A radial shape which will be cut from the singleLargeCube.
-}
smallRadius = [(Radius x) | x <- [2,2..]]
smalAngles = map Angle [0,45..360]

cutterCubes :: ExceptT BuilderError (State CpointsStack ) CpointsList
cutterCubes = do
  --let largeCube = (evalState $ runExceptT singleLargeCube) []

  btmFaces <-
    buildCubePointsListWithAdd "btmFaces"
    (createBottomFaces (Point (5) (-10) 0) smallRadius smalAngles )
    idList
   
  topFaces <-
    buildCubePointsListWithAdd "topFaces"
    (createTopFaces (Point (5) (-10) 10) smallRadius smalAngles )
    idList

  cutterCubes <-
    buildCubePointsListWithAdd "cutterCubes"
    btmFaces
    topFaces
    
  return cutterCubes

generatecutterCubesToCxForErrors :: IO ()
generatecutterCubesToCxForErrors  =
  let initialState = []
      
  in  print $ show  (    (evalState $ runExceptT cutterCubes)    initialState)

generatecutterCubesStl :: CpointsStack -> IO ()
generatecutterCubesStl inState = 
  let cpoints =  ((execState $ runExceptT (cutterCubes )) inState)
  in  writeStlToFile $ newStlShape "cutter cubes"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)





{----------------------------------------------------- build the union ---------------------------------------------
Combine the single large cube and the cutter.
-}
unionCubes :: ExceptT BuilderError (State CpointsStack ) CpointsList
unionCubes = do
  ------------build the containing cube from which the shape will be removed--------------
  btmContainingFaces <-
    buildCubePointsListWithAdd "btmFaces"
    (createBottomFaces (Point 0 0 0) largeRadius angles )
    idList
   
  topContainingFaces <-
    buildCubePointsListWithAdd "topFaces"
    (createTopFaces (Point 0 0 10) largeRadius angles )
    idList

  containingCube <-
    buildCubePointsListWithNoAdd "containingCube"
    btmContainingFaces
    topContainingFaces

  ------------------------------- build the cutter cubes for cutting the radial shape----------
  btmCutterFaces <-
    buildCubePointsListWithAdd "btmFaces"
    (createBottomFaces (Point (5) (-10) 0) smallRadius smalAngles )
    idList
   
  topCutterFaces <-
    buildCubePointsListWithAdd "topFaces"
    (createTopFaces (Point (5) (-10) 10) smallRadius smalAngles )
    idList

  cutterCubes <-
    buildCubePointsListWithNoAdd "cutterCubes"
    btmCutterFaces
    topCutterFaces

  --extract the cutter cubes front faces and convert to back faces and then reverse normals so they can be combined (+++)
  --with the inner faces of the large containing cube.
  cutterFaces <- buildCubePointsListWithNoAdd "cutterFaces"
       (map (reverseNormal . toBackFace . extractFrontFace) cutterCubes)
       idList

  let
    --curry in the cutterFaces so faces can now be accessed with a lens.
    getCutterFace = getCornerPointsWithIndex "bad cutterFace index" cutterFaces

  cube0  <- buildCubePointsListWithAdd "cube0"
            [ extractFrontFace $ head containingCube]
            [getCutterFace 0]
  
  cube1 <- buildCubePointsListWithAdd "cube1"
           [toFrontFace . extractFrontLeftLine $ head containingCube]
           --
           [getCutterFace 1]
  
  cube2 <- buildCubePointsListWithAdd "cube2"
           [toFrontFace . extractFrontLeftLine $ head containingCube]
           [getCutterFace 2]
  
  cube3 <- buildCubePointsListWithAdd "cube3"
           [toFrontFace . extractLeftFace $ head containingCube]
           [getCutterFace 3]
  
  cube4 <- buildCubePointsListWithAdd "cube4"
           [frontFaceFromBackFace . extractBackFace  $ head containingCube]
           [getCutterFace 4]
  
  cube5 <- buildCubePointsListWithAdd "cube5"
           [frontFaceFromBackFace . extractBackFace  $ head containingCube]
           [getCutterFace 5]
  
  cube6 <- buildCubePointsListWithAdd "cube6"
           [toFrontFace . extractRightFace $ head containingCube]
           [getCutterFace 6]

  cube7 <- buildCubePointsListWithAdd "cube7"
           [toFrontFace . extractFrontRightLine $ head containingCube]
           [getCutterFace 7]
  
  
  return cube7


generateUnionCubesToCxForErrors :: IO ()
generateUnionCubesToCxForErrors  =
  let initialState = []
      
  in  print $ show  (    (evalState $ runExceptT unionCubes)    initialState)


generateUnionCubesStl ::  IO ()
generateUnionCubesStl  = 
  let initialState = []
      cpoints =  ((execState $ runExceptT (unionCubes )) initialState)
  in  writeStlToFile $ newStlShape "cutter cubes"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
