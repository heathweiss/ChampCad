{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GMSH.Curve(buildCurves) where
{- |
Lines, in gmsh, can be made in various different ways including:
1: Line: from 2 points, which make up the 2 ends of the line.
2: Circle: from 3 points, 2 are end points, and the 3rd is a point from which a curve is calculated between the 2 end points.
Each should be a constructor of the same ADT, so that as a [GPoint] is processed, it can decide how to handle the current point.
A Line point would be combined with the prev point and made into a gmsh 'Line'.
A Circle point would be combined with the prev and next point and made into a gmsh 'Circle'

This module will start by considering system 1. Perhaps the differences will require that a separate module be
made for system 2.

A [Curve] will be generated from a [GPointId]. The [GPointId] was in turn gen'd from a NonOverLappedClosedPoints, which
is a [Point] that is non-overlapped and closed. See GMSH.Point for the definition of that. Do I need to go through the step
of ensuring the [GPointId] is still in that state? Or can I ensure//assume that it is?

Assuming the [GPointId] is non-overlapped//closed:
Traverse the [GPointId], getting an Id from State, and put it into a ADT along with the 2 GPointId's.

-}

import qualified GMSH.State as GST
import qualified GMSH.Builder.Base as GBB
import qualified Helpers.FileWriter as FW
import qualified TypeClasses.Showable as TS
import qualified GMSH.CurvePoints as CurvePoints
import qualified GMSH.Base as GB

import qualified Control.Monad.Except as E
import qualified Control.Monad.Trans.Except as TE
import qualified System.IO as SIO
import qualified Control.Monad.State.Lazy as SL
import qualified Data.Text as T 

import  Data.Data
import  Data.Typeable

import Control.Lens

makeLenses ''GST.BuilderStateData

{- |
Data for  gmsh curves which includes: Line, Circle, Bezier ...
So far only Line has been handled.
ToDo: Change this into Curves, which is how gmsh categorizes them.
      Should also change the module into Curves.
-}
data Curve =
  -- | A straight line made up of 2 end points used by gmsh. Corresponds to 'Line'.
  Line
    {_line_Id :: GST.LineId, --need to be a type from GMSH.State, the way GPointId is.
     _line_gPointIdStart :: GST.GPointId,
     _line_gPointIdEnd :: GST.GPointId
    }
    |
    Circle
    {_circle_Id :: Int, --need to be a type from GMSH.State, the way GPointId is.
     _circle_gPointIdStart :: GST.GPointId,
     _circle_gPointIdEnd :: GST.GPointId,
     _circle_gPointIdCurve :: GST.GPointId
    }



{- |
-----Given-----
errMsg::String
-supply message from calling function, such as function location.

(gpoint:gpoints)::[GPoints]
-GPoints that are being used to construct the [Curve].

builderStateData:: BuilderStateData.
The ADT for State.

-----Return-----
([Curve],BuilderStateData) where
[Curve] that was created from the [GPoint]
BuilderStateData which has had the CurveId modifed by extracting a CurveId for each Curve created.

or error:
Left error message for following reasons:
The initial Curve was a CircleArcPoint, but gmsh requires it to be preceded by an EndPoint.

The [GPoints] is empty.
-}
gPointsToCurves :: String ->  [CurvePoints.CurvePoint] -> GST.BuilderStateData -> Either String ([Curve], GST.BuilderStateData)
gPointsToCurves errMsg [] _ = --Right ([], builderStateData)
  Left $ errMsg ++ " GMSH.Curves.gPointsToCurves: empty [GPoints.GPoints] passed in."
gPointsToCurves errMsg ((CurvePoints.CircleArcPoint _ _):gpoints) _ =
  Left $ errMsg ++ " GMSH.Curves.gPointsToCurves: initial [GPoints.GPoints] passed in was a CircleArcPoint. CircleArcPoint must be preceded by an EndPoint"
gPointsToCurves errMsg (gpoint:gpoints) builderStateData =
  gPointsToCurves' errMsg gpoints builderStateData gpoint []


{-
Implements gPointsToCurves with the added parameters of:
previousGPoint :: GPoints
Used to build a Curve which is always made of > 1 GPoint.

workingList :: [Curve]
The [Curve] being gen'd from the [GPoint]
-}
gPointsToCurves' :: String -> [CurvePoints.CurvePoint] -> GST.BuilderStateData -> CurvePoints.CurvePoint ->  [Curve]
                -> Either String ([Curve], GST.BuilderStateData)

--end of [gpoints] so reverse the working list and return along with BuilderStateData 
gPointsToCurves' _ [] builderStateData _ workingList = Right $ (reverse workingList, builderStateData)

--current and previous are both EndPoints so make a line.
gPointsToCurves' errMsg ((CurvePoints.EndPoint currGPointId currPoint):gpoints) builderStateData
                (CurvePoints.EndPoint prevGPointId prevPoint) workingList =
  let
    (lineId, builderStateData') = GST.getRemoveId builderStateData
  in
  gPointsToCurves'
    errMsg
    gpoints
    builderStateData'
    (CurvePoints.EndPoint currGPointId currPoint) 
    ((Line lineId prevGPointId currGPointId) : workingList)

gPointsToCurves' errMsg (unMatchedCurrentGPointConstructor:gpoints) _ unMatchedPrevGPointConstructor _ =
  --Left $ errMsg ++ " GMSH.Curves.gPointsToCurves' has unhandled pattern match for current: " ++ (GPoints.getType unMatchedCurrentGPointConstructor)
  Left $ errMsg ++ " GMSH.Curves.gPointsToCurves' has unhandled pattern match for current: " ++ (TS.showConstructor unMatchedCurrentGPointConstructor)
                ++ " previous: "
                ++ (TS.showConstructor unMatchedPrevGPointConstructor)

-- | Implements gPointsToCurves within the ExceptStackCornerPointsBuilder <whatever type> transformer stack.
-- | See gPointsToCurves for details.
buildCurves :: SIO.Handle -> String -> CurvePoints.NonOverLappedClosedCurvePoints  -> GBB.ExceptStackCornerPointsBuilder [Curve]
buildCurves h errMsg (GB.NonOverLappedClosed []) = do
  TE.throwE $ errMsg ++ " GMSH.Builder.Curves.buildCurves: empty [NonOverLappedClosedGPoints] passed in."
buildCurves h errMsg (GB.NonOverLappedClosed gpoints) = do 
  state' <- SL.get
  let maybeLines = gPointsToCurves errMsg gpoints state'
  case maybeLines of
    Right (lines, builderStateData) -> do
      E.liftIO $ writeGScriptsToFile h lines
      E.lift $ SL.state $ \_ -> (lines, builderStateData)
    Left e -> TE.throwE e
  

writeGScriptToFile :: SIO.Handle -> Curve -> IO ()
writeGScriptToFile h line =
  let
    toGScript :: Curve -> T.Text
    toGScript (Line (GST.LineId' id) (GST.GPointId' idStart) (GST.GPointId' idEnd))  =
      T.pack $
        "\nLine("  ++
          (show (id)) ++ ") = {"  ++
          (show idStart) ++ "," ++
          (show idEnd) ++
          "};"
  
  in
  FW.writeFileUtf8 h $ toGScript line


writeGScriptsToFile :: SIO.Handle -> [Curve] -> IO ()
writeGScriptsToFile h lines =
  mapM_ (writeGScriptToFile h) lines
