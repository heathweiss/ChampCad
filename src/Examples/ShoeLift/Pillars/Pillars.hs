{-# LANGUAGE TemplateHaskell            #-}

module Examples.ShoeLift.Pillars.Pillars(outerTreadRingRadius) where

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

import Control.Lens

type Height = Double
--the size of ring to insert into the tread section.
outerTreadRingRadius = Radius 20

-- ======= insertion ring which goes into tread section, and which dowel goes into
--size of the insertion ring that goes into tread section.
--Want it to fit tightly.
--For carbon fiber which has very coarse perimeters, only slightly small than outerTreadRadius.
--May have to be slightly smaller/larger for smoother materials such as pla.

outerRingRadius = Radius 19.5
--radius that fits into the tread
--20:
 --to big to fit into section. Had to power sand and beat in. Carbon fiber.\
--19.5:
  --nice fit in the cheetah filament.
--19:
  --little too small and loose fitting


innerRingRadius = Radius 16.4
--radius that dowel fits into
--16
  --too small for pillar dowel to fit in by 1 mm. Filed it and beat it in.
--16.2
  --too small. Had to be filed.
--16.5
  --bit too big and loose fitting.


 
data RingRadii =
  RingRadius
    {_treadType  :: String,
     _pillarType :: String,
     _outerRadius :: Radius,
     _innerRadius :: Radius,
     _dowelSize :: Double
    }

makeLenses ''RingRadii

cheetahCarbon =
  RingRadius
    "cheetah" "formFutura carbonFill"
    (Radius 19.5) --fits nice and tight
    (Radius 16.4) --16.4 a bit too tight
    31.8

standardRingHeight = 35

{-Build the pillar rings-}
ringBuilder :: Height -> Radius -> Radius -> ExceptStateIOCornerPointsBuilder
ringBuilder ringHeight innerRingRadius' outerRingRadius' = do
  ring <- buildCubePointsListSingle "ring"
          (
            cylinder [innerRingRadius' | i <- [1..]] [outerRingRadius' | o <- [1..]] [Angle r | r <- [0,5..360]] (Point 0 0 0) ringHeight
          )

  liftIO $ writeStlToFile $ newStlShape "pillar cylinder"   ([FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube []  ring))
  return ring

--runRingBuilder =  (runStateT $ runExceptT $ ringBuilder standardRingHeight innerRingRadius outerRingRadius) []
runRingBuilder =  (runStateT $ runExceptT $ ringBuilder standardRingHeight (cheetahCarbon^.innerRadius) (cheetahCarbon^.outerRadius)) []
