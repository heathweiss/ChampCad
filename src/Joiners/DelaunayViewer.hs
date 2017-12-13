module Joiners.DelaunayViewer() where
{-
Create some shapes to look at for building the Joiners.Delaunay module.
-}

import Primitives.Cylindrical.Walled(cylinder)

import CornerPoints.Radius(Radius(..))
import CornerPoints.Points(Point(..))
import CornerPoints.FaceExtraction(extractB1, extractB4, extractF1, extractF4)
import CornerPoints.MeshGeneration(autoGenerateEachCube, autoGenerateEachFace)
import CornerPoints.CornerPoints((+++>), (|+++|))
import CornerPoints.FaceConversions(toTopFace)
import CornerPoints.Transpose(transposeZ)

import Geometry.Angle(Angle(..))

import Joiners.Delaunay(delaunay)

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

anglesInner = ([Angle a | a <- [0,5..360]] )
  --([Angle a | a <- [0,5..300]]++ [Angle 350] )
innerCylinder = cylinder [Radius 40 | r <- [1..]] [Radius 50 | r <- [1..]] anglesInner (Point 0 0 0) 10
innerBackBottomPoints = (extractB4 $ head innerCylinder) : (map (extractB1) $ {-tail-}  innerCylinder)

anglesOuter = ([Angle a | a <- [0,10..360]])
  --([Angle a | a <- [0,10..300]]++ [Angle 350] )
outerCylinder = cylinder [Radius 60 | r <- [1..]] [Radius 80 | r <- [1..]] anglesOuter (Point 0 0 0) 10
outerFrontBottomPoints = (extractF4 $ head outerCylinder) : (map (extractF1) $ {-tail-}  outerCylinder)


d  = delaunay outerFrontBottomPoints innerBackBottomPoints [] []

btmFaces = ((head d) +++> (tail d))

cubes = (map (toTopFace) btmFaces)
        |+++|
        (btmFaces)

builder :: ExceptStackCornerPointsBuilder
builder = do
  innerBackBottomPoints' <- buildCubePointsListSingle "innerBackBottomPoints"
                            innerBackBottomPoints

  outerFrontBottomPoints' <- buildCubePointsListSingle "innerBackBottomPoints"
                            outerFrontBottomPoints

  delaunay'' <- buildCubePointsListSingle "delaunay"
                (delaunay
                   outerFrontBottomPoints'
                   innerBackBottomPoints'
                   [] []
                )

  btmFaces <- buildCubePointsListSingle "btmFaces"
           ((head delaunay'') +++> (tail delaunay'')
           )

  cubes <- buildCubePointsListWithAdd "cubes"
           (map ((transposeZ (+10)) . toTopFace) btmFaces)
           (btmFaces)

  return cubes

runBuilder :: IO ()
runBuilder = do
  let
    builder' = runExceptT builder
    cpoints = ((execState $ builder') [])
    valCpoints = ((evalState $ builder') [])

  case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ putStrLn "output from Builder was good"
          liftIO $ writeStlToFile $ newStlShape "entire geox" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
          liftIO $ putStrLn "stl should have been output"
