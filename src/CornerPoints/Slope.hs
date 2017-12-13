{-# LANGUAGE TemplateHaskell #-}
module CornerPoints.Slope(addSlope) where

import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.Points(Point(..), calculateDistance, calculateXYDistance)
import CornerPoints.Radius(Radius(..))
import CornerPoints.Create(adjustRadiusForSlope)

import Geometry.Slope(Slope(..), flatXSlope, flatYSlope, slopeAdjustedForVerticalAngle)
import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..))


import Math.Trigonometry(sinDegrees, cosDegrees, coTanDegrees)
import Math.Distance(Distance(..))

import Control.Lens

makeLenses ''Distance

{- |
Add a slope to a CornerPoionts
need to be able to figure out the radius.

-}

addSlope :: Slope -> Slope -> Angle -> Point -> CornerPoints -> CornerPoints
addSlope    xSlope   ySlope   xyAngle  origin   cpoint =
  let 
    currentSlope = slopeAdjustedForVerticalAngle xSlope ySlope (xyAngle)

    quadrantAngle = (angle $ getQuadrantAngle (xyAngle))

    extractPoint :: CornerPoints -> Point
    extractPoint (F1 p) = p
    extractPoint (F2 p) = p
    extractPoint (F3 p) = p
    extractPoint (F4 p) = p
    
                                                    
    xyRadius = Radius $  (calculateDistance origin $ extractPoint cpoint)^.distance

    adjustedRadius = radius (adjustRadiusForSlope (xyRadius) currentSlope)
    
    xAdjusted =
       let length  = (sinDegrees quadrantAngle) * adjustedRadius
           x_axis' = x_axis (origin)
       in  
                          
           case getQuadrantAngle (xyAngle) of
             (Quadrant1Angle _) -> x_axis' + length  
             (Quadrant2Angle _) -> x_axis' + length  
             (Quadrant3Angle _) -> x_axis' - length 
             (Quadrant4Angle _) -> x_axis' - length 
                                 
    yAdjusted =
       let length = (cosDegrees quadrantAngle) *  adjustedRadius
           y_axis' = y_axis (origin)
       in
               
          case getQuadrantAngle (xyAngle) of
            (Quadrant1Angle _) -> y_axis' - length  
            (Quadrant2Angle _) -> y_axis' + length  
            (Quadrant3Angle _) -> y_axis' + length  
            (Quadrant4Angle _) -> y_axis' - length 
                                 
                                 
    zAdjusted =
       let length = (radius xyRadius) * (sinDegrees (slope currentSlope))
           z_axis' = z_axis (origin)
       in
          case currentSlope of
            (PosSlope _) -> z_axis' +  length
            (NegSlope _)  -> z_axis' - length
                                 
                                 
                                                
                                   
                                 
  in       
    --composer {_cpoint = (cpointSetter (cpoint) (Point xAdjusted yAdjusted zAdjusted)), _xyRadius = (Radius adjustedRadius) }
    cpointSetter cpoint (Point xAdjusted yAdjusted zAdjusted)



--used in createCornerPointComposableSloped to get the CornerPoints constructor
cpointSetter :: CornerPoints -> Point -> CornerPoints
cpointSetter (F1 f) point' = F1 point'
cpointSetter (F2 f) point' = F2 point'
cpointSetter (F3 f) point' = F3 point'
cpointSetter (F4 f) point' = F4 point'
cpointSetter _ _ = CornerPointsError "cpointSetter missing pattern match"



