{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
module GMSH.Lines({-gPointIdsToLines,-} Line(), pattern Line', pattern Circle', gPointsToLines) where
{- |
Lines, in gmsh, can be made in 2 different ways:
1: Line: from 2 points, which make up the 2 ends of the line.
2: Circle: from 3 points, 2 are end points, and the 3rd is a point from which a curve is calculated between the 2 end points.
Each should be a constructor of the same ADT, so that as a [GPointId] is processed, it can decide how to handle the current point.
A Line point would be combined with the prev point and made into a gmsh 'Line'.
A Circle point would be combined with the prev and next point and made into a gmsh 'Circle'

This module will start by considering system 1. Perhaps the differences will require that a separate module be
made for system 2.

A [Line] will be generated from a [GPointId]. The [GPointId] was in turn gen'd from a NonOverLappedClosedPoints, which
is a [Point] that is non-overlapped and closed. See GMSH.Point for the definition of that. Do I need to go through the step
of ensuring the [GPointId] is still in that state? Or can I ensure//assume that it is?

Assuming the [GPointId] is non-overlapped//closed:
Traverse the [GPointId], getting an Id from State, and put it into a ADT along with the 2 GPointId's.

-}

import qualified GMSH.State as GST
import qualified GMSH.GPoints as GGPts
{-
import qualified GMSH.Builder.Base as GBB
import qualified GMSH.Builder.GPoints as GBGPts
import qualified GMSH.Lines as GL
import qualified GMSH.Writer.Lines as GWL
import qualified GMSH.Writer.GPoints as GWGPts

import qualified Control.Monad.Except as E
import qualified Control.Monad.Trans.Except as TE
import qualified System.IO as SIO
-}
import  Data.Data
import  Data.Typeable 


{- |
Data for  gmsh curves which includes: Line, Circle, Bezier ...
So far only Line has been handled.
ToDo: Change this into Curves, which is how gmsh categorizes them.
      Should also change the module into Curves.
-}
data Line =
  -- | A straight line made up of 2 end points used by gmsh. Corresponds to 'Line'.
  Line
    {_line_Id :: GST.LineId, --need to be a type from GMSH.State, the way GPointId is.
     _line_gPointStart :: GST.GPointId,
     _line_gPointEnd :: GST.GPointId
    }
    |
    Circle
    {_circle_Id :: Int, --need to be a type from GMSH.State, the way GPointId is.
     _circle_gPointStart :: GST.GPointId,
     _circle_gPointEnd :: GST.GPointId,
     _circle_gPointCurve :: GST.GPointId
    }

pattern Line' a b c <- Line a b c
pattern Circle' a b c d <- Circle a b c d


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
gPointsToLines :: String ->  [GGPts.GPoints] -> GST.BuilderStateData -> Either String ([Line], GST.BuilderStateData)
gPointsToLines errMsg [] _ = --Right ([], builderStateData)
  Left $ errMsg ++ " GMSH.Lines.gPointsToLines: empty [GGPts.GPoints] passed in."
gPointsToLines errMsg ((GGPts.CircleArcPoint _ _):gpoints) _ =
  Left $ errMsg ++ " GMSH.Lines.gPointsToLines: initial [GGPts.GPoints] passed in was a CircleArcPoint. CircleArcPoint must be preceded by an EndPoint"
gPointsToLines errMsg (gpoint:gpoints) builderStateData =
  gPointsToLines' errMsg gpoints builderStateData gpoint []


{-
Implements gPointsToLines with the added parameters of:
previousGPoint :: GPoints
Used to build a Curve which is always made of > 1 GPoint.

workingList :: [Curve]
The [Curve] being gen'd from the [GPoint]
-}
gPointsToLines' :: String -> [GGPts.GPoints] -> GST.BuilderStateData -> GGPts.GPoints ->  [Line]
                -> Either String ([Line], GST.BuilderStateData)

--end of [gpoints] so reverse the working list and return along with BuilderStateData 
gPointsToLines' _ [] builderStateData _ workingList = Right $ (reverse workingList, builderStateData)

--current and previous are both EndPoints so make a line.
gPointsToLines' errMsg ((GGPts.EndPoint currGPointId currPoint):gpoints) builderStateData
                (GGPts.EndPoint prevGPointId prevPoint) workingList =
  let
    (lineId, builderStateData') = GST.getRemoveId builderStateData
  in
  gPointsToLines'
    errMsg
    gpoints
    builderStateData'
    (GGPts.EndPoint currGPointId currPoint) 
    ((Line lineId prevGPointId currGPointId) : workingList)




  
  
gPointsToLines' errMsg (unMatchedCurrentGPointConstructor:gpoints) _ unMatchedPrevGPointConstructor _ =
  Left $ errMsg ++ " GMSH.Lines.gPointsToLines' has unhandled pattern match for current: " ++ (GGPts.getType unMatchedCurrentGPointConstructor)
                ++ " previous: "
                ++ (GGPts.getType unMatchedPrevGPointConstructor)



