{-# LANGUAGE OverloadedStrings #-}

module OpenSCadScriptBaseTest(openSCadScriptBaseTest) where

import Test.HUnit

import OpenSCad.Exceptions(ScriptingError(..))
import OpenSCad.Dimensions(XLength(..), newXLength, YLength(..), newYLength, ZHeight(..), newZHeight) 
import OpenSCad.Cubes(Cube(..))
import OpenSCad.Translations(TranslationArray(..))
import OpenSCad.Common(Name(..))
import OpenSCad.Polyhedron(Polyhedron(..))
import OpenSCad.ScriptBase(ToOpenScript(..), Name(..), Script(..), fromScriptingError)

import RIO
import qualified RIO.Text as T

openSCadScriptBaseTest :: IO ()
openSCadScriptBaseTest = do
 putStrLn "\nOpenSCadScriptBaseTest"
-------------------------------------------- XLength -------------------------------------------------------------
 let 
  lookAtAnXLenthScript = TestCase 
   (do
     assertEqual "valid XLength show a double"
      ("5.0") 
      (textDisplay $ toScript $ XLen 5.0)
    
   )
 _ <- runTestTT lookAtAnXLenthScript

 let 
  lookAtAXLenthErrorScript = TestCase 
   (do
     assertEqual "invalid XLength show an error msg as a script output"
      ("\nXLen is <= 0") 
      (case newXLength 0.0 of
        Right xLength -> textDisplay $ toScript xLength
        Left e        -> textDisplay $ toScript e
      )
   )
 _ <- runTestTT lookAtAXLenthErrorScript

-------------------------------------------- YLength -------------------------------------------------------------
 let 
  lookAtAnYLenthScript = TestCase 
   (do
     assertEqual "valid YLength show a double"
      ("5.0") 
      (textDisplay $ toScript $ YLen 5.0)
    
   )
 _ <- runTestTT lookAtAnYLenthScript

 let 
  lookAtAYLenthErrorScript = TestCase 
   (do
     assertEqual "invalid YLength show an error msg as a script output"
      ("\nYLen is <= 0") 
      (case newYLength 0.0 of
        Right yLength -> textDisplay $ toScript yLength
        Left e        -> textDisplay $ toScript e
      )
   )
 _ <- runTestTT lookAtAYLenthErrorScript

-------------------------------------------- ZHeight -------------------------------------------------------------
 let 
  lookAtAnZHeightScript = TestCase 
   (do
     assertEqual "valid ZHeight show a double"
      ("5.0") 
      (textDisplay $ toScript $ ZHght 5.0)
    
   )
 _ <- runTestTT lookAtAnZHeightScript

 let 
  lookAtAZHeightErrorScript = TestCase 
   (do
     assertEqual "invalid ZHeight show an error msg as a script output"
      ("\nZHght is <= 0") 
      (case newZHeight 0.0 of
        Right zHeight -> textDisplay $ toScript zHeight
        Left e        -> textDisplay $ toScript e
      )
   )
 _ <- runTestTT lookAtAZHeightErrorScript


-------------------------------------------- CubeScript -------------------------------------------------------------
 let 
  lookAtAValidCubeScript = TestCase
   (do
     let
      eitherCubes :: [Either ScriptingError Script]
      eitherCubes = [CubeScript <$> ( Cube <$> (newXLength 1) <*> (newYLength 5) <*> (newZHeight 10)),
                     (CubeScript <$> (Cube <$> (newXLength 50) <*> (newYLength 100) <*> (newZHeight 150)))
                 ]
      eitherCubesAsScript :: [Script]
      eitherCubesAsScript = map fromScriptingError eitherCubes
     assertEqual "script output of a valid cubescript"
      "\n cube([1.0, 5.0, 10.0]);\n cube([50.0, 100.0, 150.0]);"
      (mconcat $ map (textDisplay . toScript)  eitherCubesAsScript )
   )
 _ <- runTestTT lookAtAValidCubeScript

 let 
  lookAtAnInvalidCubeScript = TestCase
   (do
     let
      eitherCubes :: [Either ScriptingError Script]
      eitherCubes = [CubeScript <$> ( Cube <$> (newXLength 0) <*> (newYLength 5) <*> (newZHeight 10)),
                     (CubeScript <$> (Cube <$> (newXLength 50) <*> (newYLength 100) <*> (newZHeight 150)))
                 ]
      eitherCubesAsScript :: [Script]
      eitherCubesAsScript = map fromScriptingError eitherCubes
     assertEqual "script output of an invalid cubescript"
      "\nXLen is <= 0\n cube([50.0, 100.0, 150.0]);"
      (mconcat $ map (textDisplay . toScript)  eitherCubesAsScript )
   )
 _ <- runTestTT lookAtAnInvalidCubeScript

 let
  cutHoleFromCube = TestCase 
   (do
     assertEqual "cut a cylinder from a cube"
      "difference(){\n cube([10.0, 10.0, 10.0]);\ncylinder(r=2,h=20); \n}"
      (textDisplay $ toScript $ 
         DiffScript (RawUtf8 "cylinder(r=2,h=20); ") (CubeScript $ Cube (XLen 10)(YLen 10) (ZHght 10))  
                     
      )
   )
 _ <- runTestTT cutHoleFromCube
 return ()
 