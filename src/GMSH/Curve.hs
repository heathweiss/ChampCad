{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
{- |
Lines, in gmsh, can be made in various different ways including:
1: Line: from 2 points, which make up the 2 ends of the line.
2: Circle: from 3 points, 2 are end points, and the 3rd is a point from which a curve is calculated between the 2 end points.
Each should be a constructor of the same ADT, so that as a [GPoint] is processed, it can decide how to handle the current point.
A Line point would be combined with the prev point and made into a gmsh 'Line'.
A Circle point would be combined with the prev and next point and made into a gmsh 'Circle'

This module will start by considering system 1. Perhaps the differences will require that a separate module be
made for system 2.

A [Curve] will be generated from a [CurvePointId]. The [CurvePointId] was in turn gen'd from a NonOverLappedClosedPoints, which
is a [Point] that is non-overlapped and closed. See GMSH.Point for the definition of that. Do I need to go through the step
of ensuring the [CurvePointId] is still in that state? Or can I ensure//assume that it is?

Assuming the [CurvePointId] is non-overlapped//closed:
Traverse the [CurvePointId], getting an Id from State, and put it into a ADT along with the 2 CurvePointId's.

-}
module GMSH.Curve(buildCurves) where


import qualified GMSH.State as GST
import qualified GMSH.Writer as Writer
import qualified Helpers.FileWriter as FW
import qualified TypeClasses.Showable as Showable
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
    {_line_Id :: GST.CurveId, --need to be a type from GMSH.State, the way CurvePointId is.
     _line_curvePointIdStart :: GST.CurvePointId,
     _line_curvePointIdEnd :: GST.CurvePointId
    }
    |
    Circle
    {_circle_Id :: Int, --need to be a type from GMSH.State, the way CurvePointId is.
     _circle_curvePointIdStart :: GST.CurvePointId,
     _circle_curvePointIdEnd :: GST.CurvePointId,
     _circle_curvePointIdCurve :: GST.CurvePointId
    }
  deriving(Show, Typeable, Data)

instance Showable.Showable Curve 

instance Writer.Scriptable Curve where
  showId (Line (GST.CurveId' id) _ _) = show id 
  writeScript h (Line id curvePointIdStart curvePointIdEnd) =
    --writeGScriptToFile h (Line id curvePointIdStart curvePointIdEnd)
    let
      toGScript :: Curve -> T.Text
      toGScript (Line (GST.CurveId' id) (GST.CurvePointId' idStart) (GST.CurvePointId' idEnd))  =
        T.pack $
          "\nLine("  ++
            (show (id)) ++ ") = {"  ++
            (show idStart) ++ "," ++
            (show idEnd) ++
            "};"
    in
    FW.writeFileUtf8 h $ toGScript (Line id curvePointIdStart curvePointIdEnd)
    
  writeScript h unhandled =
    FW.writeFileUtf8 h $ T.pack $ "GMSH.Curves.writeScript: unhandled " ++ (Showable.showConstructor unhandled)  

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
curvePointsToCurves :: String ->  [CurvePoints.CurvePoint] -> GST.BuilderStateData -> Either String ([Curve], GST.BuilderStateData)
curvePointsToCurves errMsg [] _ = --Right ([], builderStateData)
  Left $ errMsg ++ " GMSH.Curves.curvePointsToCurves: empty [GPoints.GPoints] passed in."
curvePointsToCurves errMsg ((CurvePoints.CircleArcPoint _ _):gpoints) _ =
  Left $ errMsg ++ " GMSH.Curves.curvePointsToCurves: initial [GPoints.GPoints] passed in was a CircleArcPoint. CircleArcPoint must be preceded by an EndPoint"
curvePointsToCurves errMsg (gpoint:gpoints) builderStateData =
  curvePointsToCurves' errMsg gpoints builderStateData gpoint []


{-
Implements curvePointsToCurves with the added parameters of:
previousGPoint :: GPoints
Used to build a Curve which is always made of > 1 GPoint.

workingList :: [Curve]
The [Curve] being gen'd from the [GPoint]
-}
curvePointsToCurves' :: String -> [CurvePoints.CurvePoint] -> GST.BuilderStateData -> CurvePoints.CurvePoint ->  [Curve]
                -> Either String ([Curve], GST.BuilderStateData)

--end of [gpoints] so reverse the working list and return along with BuilderStateData 
curvePointsToCurves' _ [] builderStateData _ workingList = Right $ (reverse workingList, builderStateData)

--current and previous are both EndPoints so make a line.
curvePointsToCurves' errMsg ((CurvePoints.EndPoint currCurvePointId currPoint):gpoints) builderStateData
                (CurvePoints.EndPoint prevCurvePointId prevPoint) workingList =
  let
    (lineId, builderStateData') = GST.getRemoveId builderStateData
  in
  curvePointsToCurves'
    errMsg
    gpoints
    builderStateData'
    (CurvePoints.EndPoint currCurvePointId currPoint) 
    ((Line lineId prevCurvePointId currCurvePointId) : workingList)

curvePointsToCurves' errMsg (unMatchedCurrentGPointConstructor:gpoints) _ unMatchedPrevGPointConstructor _ =
  --Left $ errMsg ++ " GMSH.Curves.curvePointsToCurves' has unhandled pattern match for current: " ++ (GPoints.getType unMatchedCurrentGPointConstructor)
  Left $ errMsg ++ " GMSH.Curves.curvePointsToCurves' has unhandled pattern match for current: " ++ (Showable.showConstructor unMatchedCurrentGPointConstructor)
                ++ " previous: "
                ++ (Showable.showConstructor unMatchedPrevGPointConstructor)

-- | Implements curvePointsToCurves within the ExceptStackCornerPointsBuilder <whatever type> transformer stack.
-- | See curvePointsToCurves for details.
buildCurves :: SIO.Handle -> String -> CurvePoints.NonOverLappedClosedCurvePoints  -> GB.ExceptStackCornerPointsBuilder [Curve]
buildCurves h errMsg (GB.NonOverLappedClosed []) = do
  TE.throwE $ errMsg ++ " GMSH.Builder.Curves.buildCurves: empty [NonOverLappedClosedGPoints] passed in."
buildCurves h errMsg (GB.NonOverLappedClosed gpoints) = do 
  state' <- SL.get
  let maybeLines = curvePointsToCurves errMsg gpoints state'
  case maybeLines of
    Right (lines, builderStateData) -> do
      E.liftIO $ Writer.writeScripts h lines
      E.lift $ SL.state $ \_ -> (lines, builderStateData)
    Left e -> TE.throwE e
  

