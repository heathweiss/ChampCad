{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module GMSH.CurvePoints(buildCurveList, CurvePoint(..),NonOverLappedClosedCurvePoints(..)) where
{- |
Supply a CurvePoint ADT, and Builder function.
-}

import qualified GMSH.State as GST
import qualified TypeClasses.Showable as Showable
import qualified CornerPoints.Points as Pts
import qualified Helpers.FileWriter as FW
import qualified GMSH.Writer as Writer
import qualified GMSH.CornerPoints as GmeshCPts
import qualified GMSH.Base as GB

import  Data.Data
import  Data.Typeable

import qualified System.IO as SIO
import qualified Data.Text as T

import qualified Control.Monad.Trans.Except as TE
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E

import Control.Lens

makeLenses ''GST.BuilderStateData


{- |
A CurvePoint represents a gmsh point used by Curves.
eg: Line uses 2 endpoints.
    Circle uses 2 endpoints and a point for the size of the arc.

In order to build a [Curve] we need not only the points, but to have various types of points to correspond to endpoints, arcpoints...
This allows a [various CurvePoint constructors] to be converted into a [various Curves such as Line or Circle].
-}
data CurvePoint = EndPoint {_endPoint_id :: GST.CurvePointId, endPoint_point :: Pts.Point}
             -- | End points for Line. Start and end points for Circle.  
             | CircleArcPoint   {_cap_id :: GST.CurvePointId, cap_point :: Pts.Point}
             -- | Center point for the arc of a Circle.
  deriving(Show, Typeable, Data)

instance Showable.Showable CurvePoint

instance Writer.Scriptable CurvePoint where
  showId (EndPoint (GST.CurvePointId' id) _) = show id
  showId (CircleArcPoint (GST.CurvePointId' id) _) = show id
  writeScript h (EndPoint endPoint_id endPoint_point) =
    let
    toGScript :: GST.CurvePointId -> Pts.Point -> T.Text
    toGScript endPoint_id (Pts.Point x y z) =
      T.pack $
        "\nPoint(" ++
          (Writer.showId (EndPoint endPoint_id endPoint_point)) ++ ") = {"  ++
          (show x) ++ "," ++
          (show y) ++ "," ++
          (show z) ++ "};"
    
    in
    FW.writeFileUtf8 h $ toGScript endPoint_id endPoint_point
  
  writeScript h unhandled =
    FW.writeFileUtf8 h $ T.pack $ "GMSH.CurverPoints.writeScript: unhandled " ++ (Showable.showConstructor unhandled)  

type NonOverLappedClosedCurvePoints = GB.NonOverLappedClosed [CurvePoint]


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
        newEndPoint = curvePntConstructor (GST.getId state') p
        newWorkingList = newEndPoint : workingList

      E.liftIO $ Writer.writeScript h newEndPoint
      --Add the new CurvePointId to the working list, and reset the state with the new CurvePointId.
      E.lift $ SL.state $
        \state'' ->
          (newWorkingList,
           GST.insertCurvePointId state'' p 
          )
      buildCurveList' h errMsg points constructors newWorkingList
