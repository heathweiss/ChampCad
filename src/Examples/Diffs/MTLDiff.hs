-- {-# LANGUAGE TemplateHaskell #-}

module Examples.Diffs.MTLDiff(generateSingleLargeCubeToCxForErrors, generateSingleLargeCubeStl,
                              generatecutterCubesToCxForErrors, generatecutterCubesStl,
                              generateUnionCubesToCxForErrors, generateUnionCubesStl) where

import Control.Monad.State.Lazy
import Control.Monad.Except

import Control.Lens

import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsList,
                     CpointsStack, CpointsList)

import CornerPoints.Radius(Radius(..))
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces)
import CornerPoints.Create(Angle(..), flatXSlope, flatYSlope, Origin(..))
import CornerPoints.CornerPoints((|@+++#@|), (|+++|),  CornerPoints(..), (+++))
import CornerPoints.Points(Point(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)
import CornerPoints.FaceExtractAndConvert(getFrontFaceAsBackFace, getFrontLeftLineAsBackFace, getLeftFaceAsBackFace,
                                          getFrontRightLineAsBackFace, getRightFaceAsBackFace, getBackRightLineAsBackFace,
                                          getLeftFaceAsFrontFace, getRightFaceAsFrontFace)
import CornerPoints.FaceConversions(toBackFace, reverseNormal, toFrontFace, backFaceFromFrontFace, frontFaceFromBackFace,
                                    f12LineFromF34Line, f34LineFromF12Line)
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
    (createBottomFaces (Point 0 0 0) largeRadius angles flatXSlope flatYSlope)
    idList
   
  topFaces <-
    buildCubePointsListWithAdd "topFaces"
    (createTopFaces (Point 0 0 10) largeRadius angles flatXSlope flatYSlope)
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
    (createBottomFaces (Point (5) (-10) 0) smallRadius smalAngles flatXSlope flatYSlope)
    idList
   
  topFaces <-
    buildCubePointsListWithAdd "topFaces"
    (createTopFaces (Point (5) (-10) 10) smallRadius smalAngles flatXSlope flatYSlope)
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

left = 0
back = 1
center = 1
right = 2
front = 3

splitCubeIntoInteriorFaces :: CornerPoints -> [CornerPoints]
splitCubeIntoInteriorFaces cube =
  [
   getLeftFaceAsFrontFace cube,
   frontFaceFromBackFace $ extractBackFace cube,
   getRightFaceAsFrontFace cube,
   extractFrontFace cube
  ]

getCubeFace :: Int-> [CornerPoints] ->  [CornerPoints]
getCubeFace  index faceList = 
      case (faceList ^? element index) of
        Nothing -> [CornerPointsError "bad face lense"]
        Just a  -> [a]

--use Lense to extract a face from the cutter cubes
--Inside the monad, curry in the cutter cube faces.
getCutterFaceBase :: [CornerPoints] -> Int-> CornerPoints
getCutterFaceBase cutterFaces index = 
  case (cutterFaces ^? element index) of
    Nothing -> CornerPointsError "bad cutter lense"
    Just a  -> a

{-
Take a CornerPoints face, and create a list comprising of:
1: A face made by from combining a single line, such as making a face from the FrontLeftLine of a FrontFace
2: The original face. Do I really need this, as why not just use the orignal face w/o splitting it.
3: A face made from the line on the opposite side from step 1.

Return them as a list so they can be accessed with a lens.
Should look at doing this with a map.
-}
splitFaceInto3Faces :: CornerPoints -> [CornerPoints]
splitFaceInto3Faces (FrontFace f1 f2 f3 f4) =
  let frontFace = FrontFace f1 f2 f3 f4
  in        
      [ (extractFrontLeftLine  frontFace)
        +++
        (f34LineFromF12Line $ extractFrontLeftLine frontFace ),
        frontFace,
        (extractFrontRightLine frontFace)
        +++
        (f12LineFromF34Line $ extractFrontRightLine frontFace )
      ]

unionCubes :: ExceptT BuilderError (State CpointsStack ) CpointsList
unionCubes = do
  --pick 1st 3 items from a list using lense, to represent a large face split into faces for each of the side lines as well
  --as main face.
  
        
  btmLargeFaces <-
    buildCubePointsListWithAdd "btmFaces"
    (createBottomFaces (Point 0 0 0) largeRadius angles flatXSlope flatYSlope)
    idList
   
  topLargeFaces <-
    buildCubePointsListWithAdd "topFaces"
    (createTopFaces (Point 0 0 10) largeRadius angles flatXSlope flatYSlope)
    idList

  largeCube <-
    buildCubePointsListWithNoAdd "largeCube"
    btmLargeFaces
    topLargeFaces

  largeCubeInteriorFaces <- buildCubePointsListWithAdd "largeCubeInteriorFaces"
                   (splitCubeIntoInteriorFaces $ head largeCube)
                   idList

  largeLeftFace <- buildCubePointsListWithAdd "largeLeftFaceAsInvertedFrontFace"
                   [getLeftFaceAsFrontFace (head largeCube)]
                   idList

  largeRightFace <- buildCubePointsListWithAdd "largeRightFace"
                    [getRightFaceAsFrontFace (head largeCube)]
                    idList

  

  largeFrontFace <- buildCubePointsListWithAdd "largeFrontFace"
                    [extractFrontFace $ head largeCube]
                    idList
  {-
  Split a face into 3 faces
  eg: FrontFace has a face made from the FrontLeftLine, FrontRightLine and the original face.
  
  let splitFaceInto3Faces :: CornerPoints -> [CornerPoints]
      splitFaceInto3Faces (FrontFace f1 f2 f3 f4) =
                    let frontFace = FrontFace f1 f2 f3 f4
                    in        
                        [ (extractFrontLeftLine  frontFace)
                          +++
                          (f34LineFromF12Line $ extractFrontLeftLine frontFace ),
                          
                          frontFace,
                          
                          (extractFrontRightLine frontFace)
                          +++
                          (f12LineFromF34Line $ extractFrontRightLine frontFace )
                        ]


  largeFrontFaceList <- buildCubePointsListWithAdd "largeFrontFaceList"
                        (splitFaceInto3Faces $ head largeFrontFace)
                        idList
  
  largeBackFace <- buildCubePointsListWithAdd "largeBackFace"
                    [frontFaceFromBackFace $ extractBackFace $ head largeCube]
                    idList
-}
  largeFaces <- buildCubePointsListWithAdd "largeFaces"
                (splitCubeIntoInteriorFaces $ head largeCube)
                idList
  ------------------------------- build the cutter cubes for cutting the radial shape----------
  btmFaces <-
    buildCubePointsListWithAdd "btmFaces"
    (createBottomFaces (Point (5) (-10) 0) smallRadius smalAngles flatXSlope flatYSlope)
    idList
   
  topFaces <-
    buildCubePointsListWithAdd "topFaces"
    (createTopFaces (Point (5) (-10) 10) smallRadius smalAngles flatXSlope flatYSlope)
    idList

  cutterCubes <-
    buildCubePointsListWithNoAdd "cutterCubes"
    btmFaces
    topFaces

  --convert cutter cubes top back faces and reverse normals so they can be combined
  --with the inner faces of the large surrounding cube.
  cutterCubesAsBackFaces <- buildCubePointsListWithNoAdd "cutterCubesAsBackFaces"
       (map (reverseNormal . getFrontFaceAsBackFace) cutterCubes)
       idList

  let
    --curry in the cutterCubesAsBackFaces so faces can now be accessed with a lens.
    getCutterFace = getCutterFaceBase cutterCubesAsBackFaces

    
  
  cube0 <- buildCubePointsListWithAdd "cube0"
           (getCubeFace front largeCubeInteriorFaces )
           [getCutterFace 0]

  
  cube1 <- buildCubePointsListWithAdd "cube1"
           ( getCubeFace left
             $ splitFaceInto3Faces
             $ head (getCubeFace front largeCubeInteriorFaces )
           )
           [getCutterFace 1]

  cube2 <- buildCubePointsListWithAdd "cube2"
           ( getCubeFace left
             $ splitFaceInto3Faces
             $ head (getCubeFace front largeCubeInteriorFaces )
           )
           [getCutterFace 2]
  
  cube3 <- buildCubePointsListWithAdd "cube3"
           (getCubeFace left largeCubeInteriorFaces )
           [getCutterFace 3]
  
  cube4 <- buildCubePointsListWithAdd "cube4"
           (getCubeFace back largeCubeInteriorFaces )
           [getCutterFace 4]

  cube5 <- buildCubePointsListWithAdd "cube5"
           --largeBackFace
           (getCubeFace back largeCubeInteriorFaces )
           [getCutterFace 5]

  cube6 <- buildCubePointsListWithAdd "cube6"
           --largeRightFace
           (getCubeFace right largeCubeInteriorFaces )
           [getCutterFace 6]
  
  cube7 <- buildCubePointsListWithAdd "cube7"
           --(getLargeFace largeFrontFaceList right )
           ( getCubeFace right
             $ splitFaceInto3Faces
             $ head (getCubeFace front largeCubeInteriorFaces )
           )
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
