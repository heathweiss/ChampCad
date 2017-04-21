{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-} --for the example on ErrorT
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

{-ToDo:
Get rid of the anything to do with my custom state monad.

Try out a StateT example, then implement it for the Except work with CornerPoints.
-}
module BuilderMonadTest(builderMonadTest) where

import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsList, buildCubePointsListSingle,
                     CpointsStack, CpointsList)


import CornerPoints.CornerPoints((|@+++#@|), (|+++|), CornerPoints(..), (+++), (+++>),
                                 cornerPointsError, findCornerPointsError)
import CornerPoints.Points(Point(..))
import CornerPoints.FaceConversions(upperFaceFromLowerFace, backFaceFromFrontFace )
import CornerPoints.FaceExtraction(extractFrontFace, extractTopFace, extractBottomFace, extractRightFace)
import CornerPoints.Transpose(transposeX, transposeY, transposeZ)
import CornerPoints.MeshGeneration(autoGenerateEachCube, autoGenerateEachFace)

import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)

import Control.Lens

import Test.HUnit hiding (State)

import Data.List(find)

--for the example on ErrorT
import qualified Data.Text as Txt
import qualified Data.Text.IO as T
import qualified Text.Read as TR
import Data.Map as Map hiding (map, filter)
import qualified Data.Map as M (map, filter)
import Control.Applicative

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader

--create a test cube b1            f1            b4            f4
btmFace = BottomFace (Point 0 0 0) (Point 0 5 0) (Point 5 0 0) (Point 5 5 0 )
cube = btmFace +++ (upperFaceFromLowerFace $ transposeZ (+1) btmFace )


builderMonadTest = do
  putStrLn ""
  putStrLn "BuilderMonadTest"
  runTestTT singleGoodCubeTest
  runTestTT singleGoodCubePushedTest
 
  
singleGoodCubeTest  = TestCase $ assertEqual
        "create a single good cube and look at its state."
        (Right [CubePoints {f1 = Point {x_axis = 0.0, y_axis = 5.0, z_axis = 0.0},
                            f2 = Point {x_axis = 0.0, y_axis = 5.0, z_axis = 1.0},
                            f3 = Point {x_axis = 5.0, y_axis = 5.0, z_axis = 1.0},
                            f4 = Point {x_axis = 5.0, y_axis = 5.0, z_axis = 0.0},
                            b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                            b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
                            b3 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 1.0},
                            b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 0.0}}])
        (let btmFace = BottomFace (Point 0 0 0) (Point 0 5 0) (Point 5 0 0) (Point 5 5 0 )
             cube = [btmFace +++ (upperFaceFromLowerFace $ transposeZ (+1) btmFace )]
             makeCube :: ExceptT BuilderError (State CpointsStack ) CpointsList
             makeCube = do
               cube' <- buildCubePointsListSingle "build a single good cube" cube
               return cube'
         in
            ((evalState $ runExceptT makeCube) [])

        )

singleGoodCubePushedTest  = TestCase $ assertEqual
        "create a single good cube and push it onto the stack."
        ( [CubePoints {f1 = Point {x_axis = 0.0, y_axis = 5.0, z_axis = 0.0},
                            f2 = Point {x_axis = 0.0, y_axis = 5.0, z_axis = 1.0},
                            f3 = Point {x_axis = 5.0, y_axis = 5.0, z_axis = 1.0},
                            f4 = Point {x_axis = 5.0, y_axis = 5.0, z_axis = 0.0},
                            b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                            b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
                            b3 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 1.0},
                            b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 0.0}}])
        (let btmFace = BottomFace (Point 0 0 0) (Point 0 5 0) (Point 5 0 0) (Point 5 5 0 )
             cube = [btmFace +++ (upperFaceFromLowerFace $ transposeZ (+1) btmFace )]
             makeCube :: ExceptT BuilderError (State CpointsStack ) CpointsList
             makeCube = do
               cube' <- buildCubePointsListSingle "build a single good cube" cube
               return cube'
         in
            ((execState $ runExceptT makeCube) [])

        )
{------------------------------- simple stack of cubes ---------------------------------}

--curry in the stack pushing function
buildCubePointsList' = buildCubePointsList (++)

reportCubePointsExceptTStateA :: IO ()
reportCubePointsExceptTStateA = do
  print $ show $ (evalState $ runExceptT cubePointsWithExceptTStateTest )  []

reportCubePointsExceptTStateS :: IO ()
reportCubePointsExceptTStateS = do
  print $ show $ (execState $ runExceptT cubePointsWithExceptTStateTest )  []

reportCubePointsExceptTStateRunState :: IO ()
reportCubePointsExceptTStateRunState = do
  print $ show $ (runState $ runExceptT cubePointsWithExceptTStateTest )  []

--run in repl with: reportCornerPointsExceptTStateA(S)(RunState)
cubePointsWithExceptTStateTest :: ExceptT BuilderError (State CpointsStack ) CpointsList
cubePointsWithExceptTStateTest = do
  list1  <- buildCubePointsList' "list 1"  [btmFace] [CornerPointsId]--Will not go on stack as not CubePoints
  list1a <- buildCubePointsList' "list 1a" list1     (map (upperFaceFromLowerFace . (transposeZ (+1))) list1) --good cube
  list2  <- buildCubePointsList' "list 2"  list1a    (map (extractTopFace . (transposeZ (+1))) list1a ) --good cube
  list3  <- buildCubePointsList' "list 3"  list2    (map (extractTopFace . (transposeZ (+1))) list2 ) --good cube
  --list3  <- buildCubePointsList' "list 3"  list2     [cube] --bad cube
  return list3

--tests out forEachCube on the repl
--use it in generateStl
runAutoGenerateEachCube inState =
  autoGenerateEachCube [] ((execState $ runExceptT cubePointsWithExceptTStateTest ) inState)
  

generateStl =
  let faces = runAutoGenerateEachCube []
      triangles' = [FacesAll | x <- [1..]] |+++^| faces
  in
  writeStlToFile $ newStlShape "stack of cubes" triangles'
  


{----------------------------------- give me an H------------------------------------------}

buildHShape :: ExceptT BuilderError (State CpointsStack ) CpointsList
buildHShape = do
  btmLeftLeg1  <- buildCubePointsList' "btmLeftLeg1"  [btmFace] [CornerPointsId]
  btmLeftLeg <- buildCubePointsList' "btmLeftLeg" btmLeftLeg1  (map (upperFaceFromLowerFace . (transposeZ (+10))) btmLeftLeg1)
  centerLeftLeg  <- buildCubePointsList' "centerLeftLeg"  btmLeftLeg    (map (extractTopFace . (transposeZ (+5))) btmLeftLeg )
  topLeftLeg  <- buildCubePointsList' "topLeftLeg"  centerLeftLeg    (map (extractTopFace . (transposeZ (+10))) centerLeftLeg )
  center  <- buildCubePointsList' "center"  centerLeftLeg (map (extractRightFace . (transposeX (+15))) centerLeftLeg )
  centerRightLeg <- buildCubePointsList' "centerRightLeg" center (map (extractRightFace . (transposeX (+5))) center)
  btmRightLeg <- buildCubePointsList' "btmRightLeg" centerRightLeg (map (extractBottomFace . (transposeZ ((-)10))) centerRightLeg)
  topRightLeg <- buildCubePointsList' "topRightLeg" centerRightLeg (map (extractTopFace . (transposeZ (+10))) centerRightLeg)
  return topRightLeg

runH inState =
  autoGenerateEachCube [] ((execState $ runExceptT buildHShape ) inState)

generateHStl =
  let faces = runH []
      triangles' = [FacesAll | x <- [1..]] |+++^| faces
  in
  writeStlToFile $ newStlShape "stack of cubes" triangles'
