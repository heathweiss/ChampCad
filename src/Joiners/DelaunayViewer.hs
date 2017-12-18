{-
Create some shapes to look at for building the Joiners.Delaunay module.
-}

module Joiners.DelaunayViewer() where

import Primitives.Cylindrical.Walled(cylinder)

import CornerPoints.Radius(Radius(..))
import CornerPoints.Points(Point(..))
import CornerPoints.FaceExtraction(extractB1, extractB4, extractF1, extractF4, extractBackLeftLine, extractBackRightLine,
                                  extractFrontLeftLine, extractFrontRightLine)
import CornerPoints.MeshGeneration(autoGenerateEachCube, autoGenerateEachFace)
import CornerPoints.CornerPoints((+++>), (|+++|))
import CornerPoints.FaceConversions(toTopFace)
import CornerPoints.Transpose(transposeZ)

import Geometry.Angle(Angle(..))

import Joiners.Delaunay(delaunay, delaunayA)

import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)

import Builder.Monad (BuilderError(..),
                      cornerPointsErrorHandler, buildCubePointsList, buildCubePointsListWithAdd,
                      buildCubePointsListSingle, buildCubePointsListSingleNoPush,
                       buildCubePointsListWithIOCpointsListBase,
                      CpointsStack, CpointsList, ExceptStackCornerPointsBuilder)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy hiding (get)
import qualified Control.Monad.State.Lazy as ST (get)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader



{-
Cut a cylinder out of another cylinder.
Done using only the bottom points: B1, B4, F1, F4.
Inner cylinder is every 5 degrees, while outer is every 10 degrees.
Uses the delaunay joiner to join the mismatched degrees.

Bottom faces are build from the bottom points, and then a top must be added to get the
final walled cylinder.
-}
bottomPointsBuilder :: ExceptStackCornerPointsBuilder
bottomPointsBuilder = do
  let
  {-----------------Build the 2 cylinders, and extract required bottom points to be joined as [BottomFace]-------------}
  --Create the inner cylinder which will supply the [B<1/4>] used to build the inner wall.
  innerBackBottomPoints <- buildCubePointsListSingle "innerBackBottomPoints"
                            
                            (let
                                --build the inner cylinder
                               angles = ([Angle a | a <- [0,5..360]] )
                               cylinder' = cylinder [Radius 40 | r <- [1..]] [Radius 50 | r <- [1..]] angles (Point 0 0 0) 10
                             in
                              --extract all the Back points. Note that 1st the first cube needs B4 and B1 extracted.
                              (extractB4 $ head cylinder') : (map (extractB1) cylinder')
                            )

  --Create the outer cylinder which supplies the [F<1/4>] used to build the outer wall.
  outerFrontBottomPoints <- buildCubePointsListSingle "innerBackBottomPoints"
                            (let
                              --build the outer cylinder
                               anglesOuter = ([Angle a | a <- [0,10..360]])
                               outerCylinder = cylinder [Radius 60 | r <- [1..]] [Radius 80 | r <- [1..]] anglesOuter (Point 0 0 0) 10
                             in
                             --extract all the Front points. Note that the first cube needs F4 and F1 extracted.
                              (extractF4 $ head outerCylinder) : (map (extractF1) $ {-tail-}  outerCylinder)
                            )




  
  --Use delaunay joiner to join the <innerBack/outerFront>Points into a [Bottom<Left/Right>Line]
  bottomLeftRightLines <- buildCubePointsListSingle "delaunay"
                (delaunay
                   outerFrontBottomPoints
                   innerBackBottomPoints
                   [] []
                )
  --create the [BottomFace] by: (head bottomLeftRightLines) +++> (tail bottomLeftRightLines)
  btmFaces <- buildCubePointsListSingle "btmFaces"
           ((head bottomLeftRightLines) +++> (tail bottomLeftRightLines)
           )

  --add top faces to the btmFaces to get cubes.
  cubes <- buildCubePointsListWithAdd "cubes"
           (map ((transposeZ (+10)) . toTopFace) btmFaces)
           (btmFaces)

  return cubes

runBottomPointsBuilder :: IO ()
runBottomPointsBuilder = do
  let
    builder' = runExceptT bottomPointsBuilder
    cpoints = ((execState $ builder') [])
    valCpoints = ((evalState $ builder') [])

  case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ putStrLn "output from Builder was good"
          liftIO $ writeStlToFile $ newStlShape "entire geox" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
          liftIO $ putStrLn "stl should have been output"


{-
Cut 1 cylinder out of another by using outer faces of bigger cylinder and inner faces of inner cylinder.
This will mean:
outer cylinder:
-extract front faces
 -extract FrontRightLine of 1st cube, and [FrontLeftLine] of all cubes

inner cylinder:
-extract back faces
 -extract BackRightLine of 1st cube, and [BackLeftLine] of all cubes.
-}

frontBackFacesBuilder :: ExceptStackCornerPointsBuilder
frontBackFacesBuilder = do
  let
  {-----------------Build the 2 cylinders, and extract required bottom points to be joined as [BottomFace]-------------}
  --Create the inner cylinder which will supply the [B<1/4>] used to build the inner wall.
  backFaces <- buildCubePointsListSingle "back faces"
                            
                            (let
                                --build the inner cylinder
                               angles = ([Angle a | a <- [0,5..360]] )
                               cylinder' = cylinder [Radius 40 | r <- [1..]] [Radius 50 | r <- [1..]] angles (Point 0 0 0) 100
                             in
                              --extract all the Back points. Note that 1st the first cube needs B4 and B1 extracted.
                              (extractBackRightLine $ head cylinder') : (map (extractBackLeftLine) cylinder')
                            )
  
  --Create the outer cylinder which supplies the [F<1/4>] used to build the outer wall.
  frontFaces <- buildCubePointsListSingle "frontFaces"
                            (let
                              --build the outer cylinder
                               anglesOuter = ([Angle a | a <- [0,10..360]])
                               outerCylinder = cylinder [Radius 60 | r <- [1..]] [Radius 80 | r <- [1..]] anglesOuter (Point 0 0 0) 100
                             in
                             --extract all the Front points. Note that the first cube needs F4 and F1 extracted.
                              (extractFrontRightLine $ head outerCylinder) : (map (extractFrontLeftLine) $ {-tail-}  outerCylinder)
                            )




  
  --Use delaunay joiner to join the <innerBack/outerFront>Points into a [Bottom<Left/Right>Line]
  frontBackFaces <- buildCubePointsListSingle "frontBackFaces"
              
                (delaunayA
                   frontFaces
                   backFaces
                   [] []
                )
              
  {-
  --create the [BottomFace] by: (head bottomLeftRightLines) +++> (tail bottomLeftRightLines)
  cubes <- buildCubePointsListSingle "btmFaces"
           ((head frontBackFaces) +++> (tail frontBackFaces)
           )

  
  -}
  return frontBackFaces


runFrontBackFacesBuilder :: IO ()
runFrontBackFacesBuilder = do
  let
    builder' = runExceptT frontBackFacesBuilder
    cpoints = ((execState $ builder') [])
    valCpoints = ((evalState $ builder') [])

  case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ putStrLn "output from Builder was good"
          liftIO $ writeStlToFile $ newStlShape "entire geox" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
          liftIO $ putStrLn "stl should have been output"

showValFrontBackFacesBuilder :: IO ()
showValFrontBackFacesBuilder = do
  let
    builder' = runExceptT frontBackFacesBuilder
    cpoints = ((execState $ builder') [])
    valCpoints = ((evalState $ builder') [])

  case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ print $ show a
