{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TemplateHaskell #-}
module CornerPoints.Composable (createCornerPoint, Origin(..), {-createTopFaces,-} Composable(..),
                                composableDefault, runComposer, createCornerPointComposable, createBottomFacesComposable, createTopFacesComposable,
                                createCornerPointComposableSloped, createComposable, addSlope, {-createTopFacesSloped,-} createBottomFacesSloped) where



import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (+++>), (|+++|), (|@+++#@|))
import CornerPoints.Transpose (transposeZ)
import CornerPoints.Radius(Radius(..))
import CornerPoints.Create(Angle(..), Slope(..), getQuadrantAngle, slopeAdjustedForVerticalAngle, adjustRadiusForSlope)


import Geometry.CornerPoints(squaredOffAdjustmentFunction)
import  Geometry.Radius(calcultateDistance)

import Math.Trigonometry(sinDegrees, cosDegrees)

import Control.Lens

{- |

Reproduce CornerPoints.Create but with a way of using function composition to
geometrically manipulate the shapes.:




This module may(should?) eventually replace CornerPoints.Create or some parts of it.

Example: CornerPoints.Create.createCornerPoint takes in Slope info even if it is not required.
This module would allow Slope to be added via composition.

Example: CornerPoints.Create.createCornerPointSquaredOff combines Slope and squared off function.
This should be done with a combination of: manipulate the [Radius] then create the slope shape.

-}

type Power = Double
type Origin = Point


data Composable = Composable {_cpoint :: CornerPoints , _xyRadius :: Radius, _xyAngle :: Angle, _origin :: Origin}
  deriving (Show, Eq)
makeLenses ''Composable

composableDefault = Composable {_cpoint = F1 (Point 0 0 0), _xyRadius = Radius 0, _xyAngle = Angle 0, _origin = Point 0 0 0 }

runComposer :: Composable -> CornerPoints
runComposer composer = composer^.cpoint 

{- |
Create a Composable that uses slope.
-}
createCornerPointComposableSloped :: Slope -> Slope -> Composable  -> Composable
createCornerPointComposableSloped xSlope      ySlope   composer =
  let 
     currentSlope = slopeAdjustedForVerticalAngle xSlope ySlope (composer^.xyAngle)
                                                                                      
     radiusAdjustedForSlope = radius (adjustRadiusForSlope (composer^.xyRadius) currentSlope)
    
     quadrantAngle = (angle $ getQuadrantAngle (composer^.xyAngle))
    
     --used to adjust the current point x/y values, according to the logic of the passed in
     --adjuster function fx. Eg: a function that squares off the shape.
     --rename to adjustedRadius
     adjustedRadius = radiusAdjustedForSlope
     {-
     adjustedRadius = radiusAdjuster
                        radiusAdjustedForSlope
                        ((sinDegrees quadrantAngle) * radiusAdjustedForSlope) --getX
                        ((cosDegrees quadrantAngle) * radiusAdjustedForSlope)--getY
     -}
     xAdjusted =
       let length  = (sinDegrees quadrantAngle) * adjustedRadius
           x_axis' = x_axis (composer^.origin)
       in  
                          
           case getQuadrantAngle (composer^.xyAngle) of
             (Quadrant1Angle _) -> x_axis' + length  
             (Quadrant2Angle _) -> x_axis' + length  
             (Quadrant3Angle _) -> x_axis' - length 
             (Quadrant4Angle _) -> x_axis' - length 
                                 
     yAdjusted =
       let length = (cosDegrees quadrantAngle) *  adjustedRadius
           y_axis' = y_axis (composer^.origin)
       in
               
          case getQuadrantAngle (composer^.xyAngle) of
            (Quadrant1Angle _) -> y_axis' - length  
            (Quadrant2Angle _) -> y_axis' + length  
            (Quadrant3Angle _) -> y_axis' + length  
            (Quadrant4Angle _) -> y_axis' - length 
                                 
                                 
     zAdjusted =
       --let length = (radius (composer^.xyRadius)) * (sinDegrees (slope currentSlope))
       let length = adjustedRadius * (sinDegrees (slope currentSlope))
           z_axis' = z_axis (composer^.origin)
       in
          case currentSlope of
            (PosSlope _) -> z_axis' +  length
            (NegSlope _)  -> z_axis' - length
                                 
                                 
                                                
                                   
                                 
  in       
    composer {_cpoint = (cpointSetter (composer^.cpoint) (Point xAdjusted yAdjusted zAdjusted)), _xyRadius = (Radius adjustedRadius) }

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
    
                                                    
    xyRadius = calcultateDistance origin $ extractPoint cpoint

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

{- |
Creates a Composable datatype by using raw values, including CornerPoints constructor, to calculate the CornerPoint,
and fill other supplied values.

If the CornerPoint has already been calculated, or the CornerPoint will be calculated later from raw valued,
use createComposable instead.
-}
createCornerPointComposable :: (Point-> CornerPoints) -> Origin -> Radius ->   Angle -> Composable
createCornerPointComposable    cPoint                    origin    horizRadius verticalAngle =
  composableDefault { _cpoint = (createCornerPoint (cPoint) origin horizRadius verticalAngle),
                      _xyRadius = horizRadius,
                      _xyAngle = verticalAngle,
                      _origin = origin
                    }
{- |
Create a Composable datatype with a pre-calculated CornerPoint passed in.

Differs from createCornerPointComposable in that:
createCornerPointComposable calculates the CornerPoint using CornerPoints.Create.createCornerPoint and a CornerPoints constructor
createComposable has the CornerPoint already supplied
-}
createComposable :: CornerPoints -> Origin -> Radius ->   Angle -> Composable
createComposable    cPoint                    origin    horizRadius verticalAngle =
  composableDefault { _cpoint = cPoint,
                      _xyRadius = horizRadius,
                      _xyAngle = verticalAngle,
                      _origin = origin
                    }
{-
Create a CornerPoint from raw values, including a CornerPoints constructor.
Differs from CornerPoints.Create.createCornerPoint in that it does not use Slope.
This should eventually replace CornerPoints.Create.createCornerPoint.
-}
--ToDo: Replace CornerPoints.Create.createCornerPoint with this version so Slope is no longer required.
createCornerPoint :: (Point-> CornerPoints) -> Origin -> Radius ->  Angle -> CornerPoints
createCornerPoint cPoint origin horizRadius verticalAngle   =
                             let 
                                 --currentSlope = slopeAdjustedForVerticalAngle xSlope ySlope verticalAngle
                                                                                      
                                 --radiusAdjustedForSlope = radius (adjustRadiusForSlope horizRadius currentSlope)

                                 baseOfAngle = (angle $ getQuadrantAngle verticalAngle)
                                 sinOfVerticalAngle = sinDegrees baseOfAngle
                                 cosOfVerticalAngle = cosDegrees baseOfAngle
                                 
                                 setXaxis =
                                   --let length = radiusAdjustedForSlope * sinOfVerticalAngle
                                   let length = (radius horizRadius) * sinOfVerticalAngle
                                       x_axis' = x_axis origin
                                   in
                                      
                                    case getQuadrantAngle verticalAngle of
                                      (Quadrant1Angle _) -> x_axis' + length
                                      (Quadrant2Angle _) -> x_axis' + length
                                      (Quadrant3Angle _) -> x_axis' - length
                                      (Quadrant4Angle _) -> x_axis' - length

                                 
                                 setYaxis =
                                   --let length = radiusAdjustedForSlope * cosOfVerticalAngle
                                   let length = (radius horizRadius) * cosOfVerticalAngle
                                       y_axis' = y_axis origin
                                   in
                                     
                                    case getQuadrantAngle verticalAngle of
                                      (Quadrant1Angle _) -> y_axis' - length
                                      (Quadrant2Angle _) -> y_axis' + length
                                      (Quadrant3Angle _) -> y_axis' + length
                                      (Quadrant4Angle _) -> y_axis' - length
                                   


                                 setZaxis = z_axis origin
                                 {-
                                 setZaxis'' = 
                                   let -- length = (radius horizRadius) * (sinDegrees (slope currentSlope))
                                       z_axis' = z_axis origin
                                   in
                                    case currentSlope of
                                     --(PosSlope _) -> z_axis' +  length
                                     (PosSlope _) -> z_axis' +  horizRadius
                                     --(NegSlope _)  -> z_axis' - length
                                     (NegSlope _)  -> z_axis' - horizRadius
                                 
                                 -}
                             in       
                                 cPoint (Point setXaxis setYaxis setZaxis )



createBottomFacesSloped :: Origin -> [Radius] -> [Angle] ->  Slope -> Slope -> [CornerPoints]
createBottomFacesSloped inOrigin radii angles xSlope ySlope  =
    (addSlope xSlope ySlope (head angles) inOrigin $
     createCornerPoint
      (F4)
      inOrigin
      (head radii)
      (head angles)
      
    ) 
    +++
    B4 inOrigin
    +++>
    [(addSlope xSlope ySlope (head angles) inOrigin $
      createCornerPoint
      (F1)
      inOrigin
      radius
      angle
      
     ) 
     +++
     B1 inOrigin
       | angle <- tail angles
       | radius <- tail radii
    ]
{-
createTopFaces :: Origin -> [Radius] -> [Angle] -> [CornerPoints]
createTopFaces inOrigin radii angles   =
    (createCornerPoint
      (F3)
      inOrigin
      (head radii)
      (head angles)
      
    ) 
    +++
    B3 inOrigin
    +++>
    [(createCornerPoint
      (F2)
      inOrigin
      radius
      angle
      
     ) 
     +++
     B2 inOrigin
       | angle <- tail angles
       | radius <- tail radii
    ]
-}
{-
createTopFacesSloped :: Origin -> [Radius] -> [Angle] -> Slope -> Slope -> [CornerPoints]
createTopFacesSloped inOrigin radii angles xSlope ySlope =
   (addSlope xSlope ySlope (head angles) inOrigin $
    createCornerPoint
      (F3)
      inOrigin
      (head radii)
      (head angles)
      
    ) 
    +++
    B3 inOrigin
    +++>
    [ (addSlope xSlope ySlope angle inOrigin $
      createCornerPoint
      (F2)
      inOrigin
      radius
      angle
      
     ) 
     +++
     B2 inOrigin
       | angle <- tail angles
       | radius <- tail radii
    ]
-}
{- |
Creates [CornerPoints.BottomFace] from a [Composable]
inComposables must be made up of the proper CornerPoints types which is:
F4 : [F1]
-}
--ToDo: Should not have to pass in the proper CornerPoints type.
createBottomFacesComposable :: [Composable] -> [CornerPoints]
createBottomFacesComposable  inComposables   =
  let headCCPoint = (head inComposables)^.cpoint
  in
      ((head inComposables)^.cpoint) +++ B4 ((head inComposables)^.origin)
      +++>
      [(curr^.cpoint) +++ B1 (curr^.origin) | curr <-  (tail inComposables)]
      

{- |
Creates [CornerPoints.TopFace] from a [Composable]
inComposables must be made up of the proper CornerPoints types which is:
F3 : [F2]
-}
--ToDo: Should not have to pass in the proper CornerPoints type.
createTopFacesComposable :: [Composable] -> [CornerPoints]
createTopFacesComposable  inComposables   =
  --let headCCPoint = (head inComposables)^.cpoint
  
      ((head inComposables)^.cpoint) +++ B3 ((head inComposables)^.origin)
      +++>
      [(curr^.cpoint) +++ B2 (curr^.origin) | curr <-  (tail inComposables)]
      
