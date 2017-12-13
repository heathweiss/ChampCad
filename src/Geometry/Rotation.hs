-- {-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell #-}
module Geometry.Rotation(rotatePointAroundZAxis, rotateCornerPointAroundZAxis) where

import CornerPoints.CornerPoints(CornerPoints(..), (+++))
import CornerPoints.Points(Point(..), calculateXYDistance)
import CornerPoints.Radius(Radius(..))
import CornerPoints.Create()

import Geometry.Angle(RotateFactor, getXYAngle, Angle(..), getQuadrantAngle, rotateAngle, )
import Geometry.Vertex(getXWithQuadrant, getYWithQuadrant, Vertex(..), adjustPointAxis)

import Math.Trigonometry(sinDegrees, cosDegrees, coTanDegrees)
import Math.Distance(Distance(..))

import Control.Lens



makeLenses ''Distance
{-
[rotateFactor::RotateFactor
 origin::Point
 pointToRotate::Point
]
|
|rotateAngle
|
[rotatedAngle
 origin::Point
 pointToRotate::Point
]
|
|calculateXYDistance
|
[rotatedAngle::Angle
 xyRadius::Radius
 origin::Point
]
|
|adjustPointAxis X : getXWithQuadrant
|adjustPointAxis Y : getYWithQuadrant
|
[rotatedPoint::Point
]
-}
rotatePointAroundZAxis :: RotateFactor -> Point -> Point -> Point
rotatePointAroundZAxis rotateFactor origin pointToRotate  =
  let rotatedAngle = rotateAngle rotateFactor $ getXYAngle origin pointToRotate 
      --xyRadius = Radius $ (calculateXYDistance origin pointToRotate)^.distance
      xyRadius =
        case (calculateXYDistance origin pointToRotate) of
          (Distance d) -> Radius d
          NoDistance   -> Radius 0.0
  in
      (adjustPointAxis (getXWithQuadrant rotatedAngle xyRadius)) . (adjustPointAxis (getYWithQuadrant rotatedAngle xyRadius)) $ origin {z_axis = (z_axis pointToRotate)}
      
      
rotateCornerPointAroundZAxis :: RotateFactor -> Point -> CornerPoints -> CornerPoints
rotateCornerPointAroundZAxis rotateFactor origin (TopFace b2' f2' b3' f3')  =
  (
    (rotateCornerPointAroundZAxis rotateFactor origin $ B2 b2')
    +++
    (rotateCornerPointAroundZAxis rotateFactor origin $ B3 b3')
  )
  +++
  (
    (rotateCornerPointAroundZAxis rotateFactor origin $ F2 f2')
    +++
    (rotateCornerPointAroundZAxis rotateFactor origin $ F3 f3')
  )
{-
rotateCornerPointAroundZAxis :: RotateFactor -> Point -> CornerPoints -> CornerPoints
rotateCornerPointAroundZAxis rotateFactor origin (TopFace b2' f2' b3' f3')  =
  TopFace
   (rotatePointAroundZAxis rotateFactor origin b2')
   (rotatePointAroundZAxis rotateFactor origin f2')
   (rotatePointAroundZAxis rotateFactor origin b3')
   (rotatePointAroundZAxis rotateFactor origin f3')
-}

rotateCornerPointAroundZAxis rotateFactor origin (B1 b1')  =
  B1 (rotatePointAroundZAxis rotateFactor origin b1')

rotateCornerPointAroundZAxis rotateFactor origin (B2 b2')  =
  B2 (rotatePointAroundZAxis rotateFactor origin b2')

rotateCornerPointAroundZAxis rotateFactor origin (B3 b3')  =
  B3 (rotatePointAroundZAxis rotateFactor origin b3')

rotateCornerPointAroundZAxis rotateFactor origin (B4 b4')  =
  B4 (rotatePointAroundZAxis rotateFactor origin b4')

rotateCornerPointAroundZAxis rotateFactor origin (F1 f1')  =
  F1 (rotatePointAroundZAxis rotateFactor origin f1')

rotateCornerPointAroundZAxis rotateFactor origin (F2 f2')  =
  F2 (rotatePointAroundZAxis rotateFactor origin f2')

rotateCornerPointAroundZAxis rotateFactor origin (F3 bf')  =
  F3 (rotatePointAroundZAxis rotateFactor origin bf')

rotateCornerPointAroundZAxis rotateFactor origin (F4 bf')  =
  F4 (rotatePointAroundZAxis rotateFactor origin bf')


rotateCornerPointAroundZAxis _ _ _ =
  CornerPointsError "illegal  or unhandled CornerPoints in rotateCornerPointAroundZAxis"
