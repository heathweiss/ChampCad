{-# LANGUAGE TemplateHaskell #-}
{-
Start with a Cubical shape from Cubical.Cubical.
It is based on the Geox shoelift.
Add a set of walls to it by oversizing it and creating walls of desired thichness.
-}
module Examples.Cubical.CubicalWalls where

import Cubical.Cubical (CubicalInput(..), createXaxisLine, zDownSlope, adjustWidth)

import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..), (+++^))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import Stl.StlCornerPointsWithDegrees(FacesWithRange(..))
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints((|@+++#@|), (|+++|), CornerPoints(..), (+++), (+++>))
import CornerPoints.FaceExtraction(extractTopFace, extractBottomFace, extractFrontFace)
import CornerPoints.FaceConversions(upperFaceFromLowerFace, backFaceFromFrontFace, lowerFaceFromUpperFace )
import CornerPoints.Degree(Degree(..))
import TypeClasses.Transposable(transposeZ, transposeY, transposeX)

import Control.Lens

topSlope = 10.0

geoxOrigin = Point 50.2 0 28.1

geoxHeelRearHalfAttachmentRing = 
  let geoxDims =
        [CubeIn (B2(Point 50.2 0 28.1)) 0.0,
         CubeIn (F2(Point 37.5 1 27.7)) 20.4, 
         CubeIn (F2(Point 35.0 3 27.3)) 29.7,
         CubeIn (F2(Point 31.5 5 26.9)) 35.6,
         CubeIn (F2(Point 26.4 10 26.0)) 45.5,
         CubeIn (F2(Point 22.8 15 24.8)) 53.0,
         CubeIn (F2(Point 19.9 20 23.8)) 58.7,
         CubeIn (F2(Point 18.4 25 22.7)) 61.8,
         CubeIn (F2(Point 17.4 30 21.7)) 64.8,
         CubeIn (F2(Point 16.8 35 21.0)) 66.3,
         CubeIn (F2(Point 16.7 40 20.2)) 67.1,
         CubeIn (F2(Point 17.0 45 19.6)) 66.6,
         CubeIn (F2(Point 17.4 50 18.6)) 66.5,
         CubeIn (F2(Point 18.6 55 18.2)) 65.5,
         CubeIn (F2(Point 20.3 60 18.4)) 63.1,
         CubeIn (F2(Point 22.0 65 18.5)) 59.7,
         CubeIn (F2(Point 23.8 70 18.8)) 57.4,
         CubeIn (F2(Point 25.2 75 18.6)) 55.7,
         CubeIn (F2(Point 26.6 80 18.7)) 54.6,
         CubeIn (F2(Point 27.7 85 19.1)) 54.2,
         CubeIn (F2(Point 28.0 90 19.7)) 54.7,
         CubeIn (F2(Point 28.0 95 19.0)) 56.5,
         CubeIn (F2(Point 27.9 100 17.9)) 58.7,
         CubeIn (F2(Point 27.4 105 16.4)) 61.2,
         CubeIn (F2(Point 26.4 110 15.1)) 63.2,
         CubeIn (F2(Point 24.6 115 13.3)) 65.5,
         CubeIn (F2(Point 23.7 120 12.3)) 68.0,
         CubeIn (F2(Point 22.3 125 11.8)) 70.6,
         CubeIn (F2(Point 20.7 130 11.3)) 73.3,
         CubeIn (F2(Point 19.1 135 11.0)) 75.6,
         CubeIn (F2(Point 17.7 140 10.7)) 77.7
        ]

      ------------------------------------------------------------------------------------------leftOff--------------------------------------------------------
      {-
      Probably have to move the outer dims: transposeX (\x -> x - 4)
      so the inner dims stay centered.

      Start extracting faces and building walls.
      -}

      ------------- inner dims -----------------------------
      --keep only the 1st 60(ish) degrees
      dimsFirst60Degrees = take 18 geoxDims
      
      dimsRaised = map (transposeZ (\z -> (z + 10))) dimsFirst60Degrees

      ---------- outer dims ----------------------
      dimsWidened = map (adjustWidth (+8.0)) dimsFirst60Degrees

      --move the back of 1st cube back the thickness of the wall
      dimsSetBackHeadSmoothedWidened =
        let first = head dimsWidened
            second = head $ tail dimsWidened
            thirdToEnd = tail $ tail dimsWidened
        in
            (transposeY (\y -> y - 4) first) : (adjustWidth (\width -> width + 4) second) : thirdToEnd
       
      dimsRaisedSetBackHeadWidened  = map (transposeZ (+10.0)) dimsSetBackHeadSmoothedWidened   
      -----------------------do the inner walls--------------------
      
      facesBtmBeforeConversion = 
        (createXaxisLine (head dimsFirst60Degrees))
        +++>
        (map (createXaxisLine) (tail dimsFirst60Degrees))

      facesBtm = map (lowerFaceFromUpperFace) facesBtmBeforeConversion

      facesTop =
        (createXaxisLine (head dimsRaised))
        +++>
        (map (createXaxisLine) (tail dimsRaised))

      cubes = facesBtm |+++| facesTop

      ---------------- do outer walls -------------------------------------
      facesBtmWidenedBeforeConversion = 
        (createXaxisLine (head dimsSetBackHeadSmoothedWidened))
        +++>
        (map (createXaxisLine) (tail dimsSetBackHeadSmoothedWidened))

      facesSetBackHeadWidened = map (lowerFaceFromUpperFace) facesBtmWidenedBeforeConversion

      facesRaisedSetBackHeadWidened =
        (createXaxisLine (head dimsRaisedSetBackHeadWidened))
        +++>
        (map (createXaxisLine) (tail dimsRaisedSetBackHeadWidened))

      cubesWidened = facesSetBackHeadWidened |+++| facesRaisedSetBackHeadWidened
      

      geoxTriangles = ((FacesBackBottomLeftRightTop : [FacesBottomLeftRightTop | x <- [1,2..15]]) ++ [FacesBottomFrontLeftRightTop]) |+++^| cubesWidened

  in  writeStlToFile $ newStlShape "geox heel" geoxTriangles

