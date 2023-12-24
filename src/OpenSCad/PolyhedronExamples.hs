{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ParallelListComp #-}
{- | Show examples of using the 'OpenSCad.Polyhedron.Polyhedron' ADT -}
module OpenSCad.PolyhedronExamples() where

import RIO
import qualified RIO.Text as T
import qualified Prelude as P

import Test.HUnit

import CornerPoints.CornerPoints(CornerPoints(..), (+++))
import CornerPoints.Points(Point(..))
import CornerPoints.Transpose(transposeX, transposeY, transposeZ)
import CornerPoints.FaceConversions(toBackFace, toFrontTopLine)

import OpenSCad.ScriptBase(ToOpenScript(toScript), Name(..), Script(..))
import OpenSCad.Common(Name(..))
import OpenSCad.Polyhedron(Polyhedron(..))

{-
Create a CornerPoints cube, make a copy above it, then convert them to CornerPointsScipt.
Write the script out to temp.txt in ..\src
-}
writeMyCubeToFile :: IO ()
writeMyCubeToFile = do
 let
      btmFrontLn = BottomFrontLine (Point 0 0 0) (Point 100 0 0)
      --Make TopFrontLine by transposing btmFrontLn upwards and forward on both x & y axis.
      --This helps make it apparent in OpenSCAD, that I am working on the correct faces.
      topFrontLn = transposeX (+5) $ transposeY (+10) $ toFrontTopLine $  transposeZ (+10) btmFrontLn
      frontFace  = btmFrontLn +++ topFrontLn
      backFace   = toBackFace $ transposeY ((+15)) frontFace
      cube       = frontFace +++ backFace 
      

      topCube    = transposeZ (+ 20) cube
      cubeScripts = [PolyhedronScript $  PolyCPoints (Name ("cube" <> (display $ textDisplay x)) ) cube' 
               | x <- ([1..] :: [Int])
               | cube' <- [cube, topCube]
             ]
 writeFileUtf8Builder "temp.txt" $ mconcat $ map  toScript cubeScripts 

---------------------------------------------------------------- tests -----------------------------------------------------------------------
polyHedronTests :: IO ()
polyHedronTests = do
 P.putStrLn "======================================== polyhedron tests ================================================"
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