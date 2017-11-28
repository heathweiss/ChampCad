module Examples.ShoeLift.Pillars.Pillars(outerRingRadius) where

{- |
Create the pillar inserts that go inside the tread rings, and into which the dowels go.

Make them tall enough to screw the dowels into them, above/below the tread sections.

Terms:

pillarRing
-The actual ring which goes into the tread hole, and into which the dowel goes.

outerRingRadius
-outer radius of pillarRing.
-Perhaps the ring in the tread should be slightly bigger than the ring, so it fits, and can be glued.

innerRingRadius
-inner radius of the pillarRing. Will be the same as the dowel.
-Perhaps slightly larger than dowe. so dowel will fit.
-}

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++),(+++>),(|+++|))
import CornerPoints.MeshGeneration(autoGenerateEachCube, autoGenerateEachFace)
import CornerPoints.Radius (Radius(..))
import Geometry.Angle(Angle(..))
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces, createTopFacesVariableHeight, createBottomFacesVariableHeight)
import CornerPoints.FaceConversions(toTopFace, toBackFace, toFrontFace, toFrontLeftLine, toFrontRightLine, toBottomFace)
import CornerPoints.FaceExtraction(extractBackFace, extractBackTopLine, extractFrontTopLine, extractFrontFace, extractLeftFace,
                                   extractFrontLeftLine, extractFrontRightLine, extractBackRightLine, extractBackLeftLine, extractBottomFace,
                                   extractTopFace)

import TypeClasses.Transposable(TransposePoint, transposeX, transposeY, transposeZ, TransposeLength, transpose,
                                TransposeWithList, transposeWithList)

import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)

import Builder.ExceptStateIO(BuilderError(..),
                      cornerPointsErrorHandler, buildCubePointsList, buildCubePointsListWithAdd,
                      buildCubePointsListSingle, buildCubePointsListSingleNoPush,
                      CpointsStack, CpointsList, ExceptStateIOCornerPointsBuilder)
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

import Persistable.Radial (Layer(..), AngleHeightRadius(..), AnglesHeightsRadii(..), nameUnique', angleHeightRadius', layerId',
                           angleHeightRadiusLayerId', extractAnglesHeightsRadiiFromEntity, ExtractedAngleHeightRadius(..),
                           extractRadii, extractAngles, extractHeights, extractLayerId, extractOrigin, loadAndExtractedAngleHeightRadiusFromDB)


import  Joiners.Manual(Joiner(..),joiner, takeLeading, takeTrailing)

import Primitives.Cylindrical.Walled(cylinder)

type Height = Double
outerRingRadius = Radius 20
--20:
 --used this for the cheetah
 --gave an inner diameter of 39 in the cheetah rings
 --gave a outer diameter of 41 in the carbon fiber, which prints very course edges. This coarse edge is just for 1st few layers.
   --This may only be the 1st few layers that are coarse.
--19:
  --little too small for pillar, try 19.5

--this leaves a bit of room for the wood dowels I am using.
innerRingRadius = Radius 16
--16
  --used this for the cheetah
  --030.25 inner radius of the carbon fiber compared to 31.5 of the dowel
  --This was just 1st few layers of test ring, which turned out to be wrong as only 1st few layers on btm are rough.
--16.5
  --bit too big. 32.2
standardHeight = 35

{-Build the pillar rings-}
ringBuilder :: Height -> Radius -> Radius -> ExceptStateIOCornerPointsBuilder
ringBuilder ringHeight innerRingRadius' outerRingRadius' = do
  ring <- buildCubePointsListSingle "ring"
          (
            cylinder [innerRingRadius' | i <- [1..]] [outerRingRadius' | o <- [1..]] [Angle r | r <- [0,5..360]] (Point 0 0 0) ringHeight
          )

  liftIO $ writeStlToFile $ newStlShape "pillar cylinder"   ([FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube []  ring))
  return ring

runRingBuilder =  (runStateT $ runExceptT $ ringBuilder standardHeight innerRingRadius outerRingRadius) []
