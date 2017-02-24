module Examples.Primitives.Squared(cylinderWithSquaredRadiiStlGenerator) where

import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsListWithAdd,
                     CpointsStack, CpointsList)

import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.Radius(Radius(..), )
import CornerPoints.Create(Angle(..))
import CornerPoints.HorizontalFaces(createTopFaces,  createBottomFaces)
import CornerPoints.Points(Point(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)

import Geometry.Radius(doubleCylinderZip)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy

import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)

cylinderWithSquaredRadii ::  ExceptT BuilderError (State CpointsStack ) CpointsList
cylinderWithSquaredRadii = do
  let
    angles = [Angle x | x <- [0,10..360]]
    radii = [Radius x | x <- [10,10..]]
    
  btmFaces <- buildCubePointsListWithAdd "btmFaces"
              (createBottomFaces (Point 0 0 0) (doubleCylinderZip radii angles) angles  )
              [CornerPointsId | x <-[1..]]

  topFaces <- buildCubePointsListWithAdd "topFaces"
              (createTopFaces (Point 0 0 20) radii angles  )
              [CornerPointsId | x <-[1..]]

  cubes    <- buildCubePointsListWithAdd "cubes"
              btmFaces
              topFaces
  
  return cubes
  

cylinderWithSquaredRadiiStlGenerator :: IO ()
cylinderWithSquaredRadiiStlGenerator = do
  let cpoints = ((execState $ runExceptT (cylinderWithSquaredRadii)) [])
  writeStlToFile $ newStlShape "cpoinst"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
