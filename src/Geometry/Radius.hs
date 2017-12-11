{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Manipulate Radius using various geometric formulas, in order to change the resulting CornerPoints.

Examples: Examples.Primitives.ComposableExample

Test in test/GeometryRadiusTest
-}
module Geometry.Radius(doubleCylinderZip, doubleCylinder, squaredOff, {-calculateDistance, calculateXYDistance-}) where

import Geometry.Angle(Angle(..), getQuadrantAngle)

import CornerPoints.Radius(Radius(..))
import CornerPoints.Points(Point(..) )
import qualified CornerPoints.Points as P (calculateDistance, calculateXYDistance ) 

import Math.Trigonometry(sinDegrees, cosDegrees)
import Math.Doubles(Distance(..))

import Control.Lens

{- |
 Use cosDegrees/sinDegrees to create a double cylinder from a Radius Angle
 that would normally create a single cylinder.
 

[Radius:                                               [ Radius
 -to be adjusted                                           -adjusted for double circular pattern.
 Angle:                                                 -
 -current angle                                         
]--------------------adjust radius                 --->]
-}
doubleCylinder :: Radius -> Angle -> Radius
doubleCylinder (Radius radius') angle' =
  Radius $ ((cosDegrees (angle $ getQuadrantAngle angle')) * radius')



{- |
 Use doubleCylinder on a [Radius] [Angle] via zipWith

[[Radius]:                                               [ [Radius]
 -to be adjusted                                           -adjusted for double circular pattern.
 [Angles]:                                                 -
 -angles                                         
]--------------------zipWith doubleCylinder  ------- --->]
-}
--ToDo: create a separate version for cos/sinDegrees which are 90 degrees opposed to each other.
doubleCylinderZip :: [Radius] -> [Angle] -> [Radius]
doubleCylinderZip    radii       angles      =
      zipWith
        (doubleCylinder)
        radii
        angles


{- |
 Create a rectangle with rounded corners, by manipulating Angle Radius.
 


-}
{-
[power:                                    [current x                     [Radius:
 -how strongly to square                    current y                      -squared off
 radius:                                    radius
 -to be adjusted                            angle
 angle':
 current angle
                   ]--------------caluclate x/y-->  ]-------------------adust radius-->]
-}
squaredOff :: Power -> Radius -> Angle -> Radius
squaredOff power (Radius radius') angle' =
  let quadrantAngle = (angle $ getQuadrantAngle angle')
      --gaurd against 0 as it creates stl with nan.
      x' = (sinDegrees quadrantAngle) * radius'
      x = case (x' == 0) of
           True -> 0.01
           False -> x'
           
      y' = (cosDegrees quadrantAngle) *  radius'
      y = case (y' == 0) of
           True -> 0.01
           False -> y'
  in  Radius $
      (radius'**2)/
        (((x**power) + (y**power))**(1/power)) 

type Power = Double
{-Now these are just wrappers around fx's in CornerPoints.Points.hs which wrap them into a Radius
  Should get rid of them entirely as seldom used and do the wrapping manually.
  Or not because:
  May use a Distance type for these, which in turn will use a <Double011/DoubleD/Double2> type
-}
{-
-- | Given a 2 points, caluclate the distance between the points.
calculateDistance :: Point -> Point -> Radius
calculateDistance    point1   point2  = Radius $   (P.calculateDistance point1   point2)^.distance

calculateXYDistance :: Point -> Point -> Radius
calculateXYDistance    point1   point2  = Radius $  (P.calculateXYDistance point1   point2)^.distance



calculateDistance :: Point -> Point -> Radius
calculateDistance    point1   point2  = Radius $ P.calculateDistance point1   point2

calculateXYDistance :: Point -> Point -> Radius
calculateXYDistance    point1   point2  = Radius $ P.calculateXYDistance point1   point2
-}
