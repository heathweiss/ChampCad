{-# LANGUAGE OverloadedStrings #-}
module OpenSCadPolyhedronTests(openSCadPolyhedronTests) where

import Test.HUnit

import CornerPoints.CornerPoints(CornerPoints(..), (+++))
import CornerPoints.Points(Point(..))
import CornerPoints.Transpose(transposeX, transposeY, transposeZ)
import CornerPoints.FaceConversions(toBackFace, toFrontTopLine)

import OpenSCad.ScriptBase(ToOpenScript(toScript), Name(..), Script(..))
import OpenSCad.Common(Name(..))
import OpenSCad.Polyhedron(Polyhedron(..))

import RIO
import qualified RIO.Text as T


openSCadPolyhedronTests :: IO ()
openSCadPolyhedronTests = do
 putStrLn "\nOpenSCadPolyhedronTests"
 let 
  printPointsAsArray = TestCase
   (do
     let
      btmFrontLn = BottomFrontLine (Point 0 0 0) (Point 10 0 0)
      topFrontLn = toFrontTopLine $  transposeZ (+10) btmFrontLn
      frontFace  = btmFrontLn +++ topFrontLn
      backFace   = toBackFace $ transposeY ((+15)) frontFace
      cube       = frontFace +++ backFace
      scriptOutput = toScript $ PolyhedronScript $ PolyCPoints (Name "1") cube
     --output results to temp.txt to get the output for assertEqual.
     --writeFileUtf8Builder "temp.txt" scriptOutput 
     assertEqual "output a openscad script for a front face"
      --"[1,2,3,]"
      (
       "\npoints1 = [[0.0,0.0,0.0],[0.0,0.0,10.0],[10.0,0.0,10.0],[10.0,0.0,0.0],[0.0,15.0,0.0],[0.0,15.0,10.0],[10.0,15.0,10.0],[10.0,15.0,0.0]];" <>
       "\nfaces1 = [[0,3,7,4],  // bottom" <>
       "\n[0,1,2,3],  // front" <>
       "\n[1,5,6,2],  // top" <>
       "\n[3,2,6,7],  // right" <>
       "\n[7,6,5,4],  // back" <>
       "\n[4,5,1,0]]; // left" <>
       "\npolyhedron(1points, 1faces, convexity=10 );"

      )
      (textDisplay scriptOutput)  
   )
 _ <- runTestTT printPointsAsArray

 return ()