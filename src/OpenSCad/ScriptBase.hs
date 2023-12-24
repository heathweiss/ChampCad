{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Corresponds to ScriptBase in HasOpenSCAD project.

Get the basic imports working.
Then generate a cube from cube points
-}
module OpenSCad.ScriptBase(ToOpenScript(..), Name(..), Script(..), fromScriptingError) where

import RIO
import qualified RIO.Text as T
import qualified Prelude as P

import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.Points(Point(..))

import Test.HUnit

import OpenSCad.Exceptions(ScriptingError(..))
import OpenSCad.Dimensions(XLength(..), newXLength, YLength(..), newYLength, ZHeight(..), newZHeight) 
import OpenSCad.Cubes(Cube(..))
import OpenSCad.Translations(TranslationArray(..))
import OpenSCad.Common(Name(..))
import OpenSCad.Polyhedron(Polyhedron(..))

{- |
Purpose: 
A wrapper around other more basic types such as primitives and translations to:
be combined in lists or other data structures, to create an entire script. 

Could I replace each contstructor with a type param, and build a list?
-} 
data Script   = PolyhedronScript {polyScript :: Polyhedron}
              | CubeScript { cubeScript :: Cube} 
              | TransScripts { transcripts :: [Script], transcriptArrayy :: TranslationArray}
              | ScriptError { scriptError :: ScriptingError}
              | DiffScript {subtractMe  :: Script , subtractFromMe :: Script} -- ^ OpenSCad Difference fx.
              | RawUtf8 {rawUtf8 :: Utf8Builder}

class ToOpenScript a where
 toScript :: a -> Utf8Builder 



instance ToOpenScript XLength where
 toScript (XLen x) = display x  

instance ToOpenScript YLength where
 toScript (YLen y) = display y  

instance ToOpenScript ZHeight where
 toScript (ZHght z) = display z  

instance ToOpenScript Cube where
 toScript (Cube x y z) = "\n cube([" <> toScript x <> ", " <> toScript y <> ", " <> toScript z <> "]);"

instance ToOpenScript TranslationArray where
 toScript (TransArray x y z) = "[" <> toScript x <> ", " <> toScript y <> ", " <> toScript z <> "]"

instance ToOpenScript Point where
 toScript (Point x y z) = "[" <> toScript x <> "," <> toScript y <> "," <> toScript z <> "]"

instance ToOpenScript Double where
 toScript dbl = display $ textDisplay dbl


instance ToOpenScript Script where
  toScript (CubeScript cube) = toScript cube
  
  toScript (TransScripts scripts transArray) = "\ntranslate(" <> toScript transArray <> "){" <> (mconcat $ map toScript scripts) <> "\n}"
  toScript (ScriptError scriptError) = toScript scriptError
  toScript (PolyhedronScript (PolyCPoints (Name cPointsName) (CubePoints f1 f2 f3 f4 b1 b2 b3 b4))) = 
    "\npoints" <> cPointsName <> " = ["        
                                  <> toScript f1 <>     -- 0
                                  "," <> toScript f2 <> -- 1
                                  "," <> toScript f3 <> -- 2
                                  "," <> toScript f4 <> -- 3
                                  "," <> toScript b1 <> -- 4
                                  "," <> toScript b2 <> -- 5
                                  "," <> toScript b3 <> -- 6
                                  "," <> toScript b4 <> -- 7
                                  "];" <>
    
    "\nfaces" <> cPointsName <>  " = ["  
                                  <> "[0,3,7,4],  // bottom\n"
                                  <> "[0,1,2,3],  // front\n"
                                  <> "[1,5,6,2],  // top\n"
                                  <> "[3,2,6,7],  // right\n"
                                  <> "[7,6,5,4],  // back\n"
                                  <> "[4,5,1,0]]; // left\n"
    <> "polyhedron(" <> cPointsName <> "points, " <> cPointsName <> "faces, convexity=10 );"
  toScript (DiffScript subtractMe subtractFromMe) =
    "difference(){" <>
          (toScript subtractFromMe) <>
       "\n" <>
         (toScript subtractMe) <>
    "\n}"
  
  toScript (RawUtf8 rawUtf8) = rawUtf8


-- | So that errors can be output to the scripts, allowing easy viewing of the errors via the output script.
instance ToOpenScript ScriptingError where
  toScript ZeroXLen = "\nXLen is <= 0"
  toScript ZeroYLen = "\nYLen is <= 0"
  toScript ZeroZHght = "\nZHght is <= 0"

-- | Extract a 'ScriptingError' from 'Either' into a 'Script'
fromScriptingError :: Either ScriptingError Script -> Script
fromScriptingError (Right script) = script
fromScriptingError (Left e) = ScriptError e 



