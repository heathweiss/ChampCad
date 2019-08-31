{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module GMSH.CurvePoints(buildCurveList, CurvePoint(..),NonOverLappedClosedCurvePoints(..)) where
{- |
A CurvePoint represents a gmsh point used by Curves.
eg: Line uses 2 endpoints.
    Circle uses 2 endpoints and a point for the size of the arc.

In order to build a [Curve] we need not only the points, but to have various types of points to correspond to endpoints, arcpoints...
This allows a [CurvePoint] to be converted into a [Curves].
-}

import qualified GMSH.State as GST
import qualified TypeClasses.Showable as TS
import qualified CornerPoints.Points as Pts
import qualified Helpers.FileWriter as FW
--import qualified GMSH.Builder.Base as GBB
import qualified GMSH.CornerPoints as GmeshCPts
import qualified GMSH.Base as GB

import  Data.Data
import  Data.Typeable
import qualified TypeClasses.Showable as TS

import qualified System.IO as SIO
import qualified Data.Text as T

import qualified Control.Monad.Trans.Except as TE
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E

import Control.Lens

makeLenses ''GST.BuilderStateData

data CurvePoint = EndPoint {_endPoint_id :: GST.CurvePointId, endPoint_point :: Pts.Point}
             -- | End points for Line. Start and end points for Circle.  
             | CircleArcPoint   {_cap_id :: GST.CurvePointId, cap_point :: Pts.Point}
             -- | Center point for the arc of a Circle.
  deriving(Typeable, Data)

instance TS.Showable CurvePoint


type NonOverLappedClosedCurvePoints = GB.NonOverLappedClosed [CurvePoint]


writeGScriptToFile :: SIO.Handle -> GST.CurvePointId -> Pts.Point -> IO ()
writeGScriptToFile h gPointId point =
  let
    toGScript :: GST.CurvePointId -> Pts.Point -> T.Text
    toGScript (GST.CurvePointId' id) (Pts.Point x y z) =
      T.pack $
        "\nPoint(" ++
          (show (id)) ++ ") = {"  ++
          (show x) ++ "," ++
          (show y) ++ "," ++
          (show z) ++ "};"

  in
  FW.writeFileUtf8 h $ toGScript gPointId point


buildCurveList :: SIO.Handle
               -> String
               -> GmeshCPts.NonOverLappedClosedPoints
               -> [(GST.CurvePointId -> Pts.Point -> CurvePoint)]
               -> GB.ExceptStackCornerPointsBuilder NonOverLappedClosedCurvePoints
buildCurveList _ errMsg (GB.NonOverLappedClosed []) _ =
  TE.throwE $ errMsg ++ " GMSH.Builder.GPoints.buildCurveList: empty NonOverLappedClosedPoints [] passed in."
buildCurveList _ errMsg _ [] =
  TE.throwE $ errMsg ++ " GMSH.Builder.GPoints.buildCurveList: empty  [Curve constructor] passed in."
buildCurveList h errMsg (GB.NonOverLappedClosed points) constructors =
  buildCurveList' h errMsg points constructors []


buildCurveList' :: SIO.Handle
               -> String -> [Pts.Point]
               -> [(GST.CurvePointId -> Pts.Point -> CurvePoint)]
               -> [CurvePoint]
               -> GB.ExceptStackCornerPointsBuilder NonOverLappedClosedCurvePoints

buildCurveList' _ _ [] _ workingList = do
  let
    builder = \state' -> (GB.NonOverLappedClosed $ reverse workingList, state')
  E.lift $ SL.state $ builder

buildCurveList' _ errMsg _ [] _ = do
  TE.throwE $ errMsg ++ " GMSH.Builder.GPoints.buildCurveList': empty  [Curve constructor] passed in."

buildCurveList' h errMsg (p:points) (curvePntConstructor:constructors) workingList = do
  state' <- SL.get
  let
    --get the Maybe CurvePointId from the BuilderStateData.pointsMap
    maybe_gpoint = GST.lookupCurvePointId state' p
  case maybe_gpoint of
    Just gpoint -> do
      --pass the CurvePointId to the overlapper fx to be added to the current State value of [CurvePointId] as per rules of the overlapping fx.
      buildCurveList' h errMsg points constructors ((curvePntConstructor gpoint p) : workingList) 
    
    Nothing -> do
      --GPoint doesn't yet exsist so:
      --Extract the new CurvePointId from the State pointsIdSupply, and add to the BuilderStateData.pointsMap, along with the vertices.
      --Write the gpoint to the gmsh script file.
      
      let
        newGPoint = GST.getId state'
        newWorkingList = (curvePntConstructor newGPoint p) : workingList

      E.liftIO $ writeGScriptToFile h newGPoint p
      --Add the new CurvePointId to the working list, and reset the state with the new CurvePointId.
      E.lift $ SL.state $
        \state'' ->
          (newWorkingList,
           GST.insertCurvePointId state'' p 
          )
      buildCurveList' h errMsg points constructors newWorkingList
