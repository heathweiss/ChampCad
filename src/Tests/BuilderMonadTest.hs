{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-} --for the example on ErrorT
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

{-ToDo:
Get rid of the anything to do with my custom state monad.

Try out a StateT example, then implement it for the Except work with CornerPoints.
-}
module Tests.BuilderMonadTest(builderMonadTest) where

import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCornerPointsList, buildCubePointsList,
                     CpointsStack, CpointsList)

--import Tests.StlAutoGenerateTest(extractFaces)

import CornerPoints.CornerPoints((|@+++#@|), (|+++|), CornerPoints(..), (+++), (+++>),
                                 cornerPointsError, findCornerPointsError)
import CornerPoints.Points(Point(..))
import CornerPoints.FaceConversions(upperFaceFromLowerFace, backFaceFromFrontFace )
import CornerPoints.FaceExtraction(extractFrontFace, extractTopFace, extractBottomFace)
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

--create a test cube
btmFace = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0 )
cube = btmFace +++ (upperFaceFromLowerFace $ transposeZ (+1) btmFace )


builderMonadTest = do
  let testPlaceHolder  = TestCase $ assertEqual
        "tests will go here"
        True
        True

  runTestTT testPlaceHolder
  
{------------------------------- CornerPoints with ExceptT State---------------------------------}






reportCornerPointsExceptTStateA :: IO ()
reportCornerPointsExceptTStateA = do
  print $ show $ (evalState $ runExceptT cornerPointsWithExceptTStateTest )  []

reportCornerPointsExceptTStateS :: IO ()
reportCornerPointsExceptTStateS = do
  print $ show $ (execState $ runExceptT cornerPointsWithExceptTStateTest )  []

--run in repl with: reportCornerPointsExceptTStateA(S)
cornerPointsWithExceptTStateTest :: ExceptT BuilderError (State CpointsStack ) CpointsList
cornerPointsWithExceptTStateTest = do
  list1 <- buildCornerPointsList "list 1" [btmFace] [(upperFaceFromLowerFace $ transposeZ (+1) btmFace )] --good cube
  list2 <- buildCornerPointsList "list 2" list1 (map (extractTopFace . (transposeZ (+1))) list1 ) --good cube
  list3 <- buildCornerPointsList "list 3" list2 [cube] --bad cube
  return list3



{------------------------------- CubePoints with ExceptT State---------------------------------}

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
  writeStlToFile $ newStlShape "BlackRunnerHeel" triangles'
  


{-
Tests.StlAutoGenerateTest:
 


Tests.BuilderMonadTest
autoGenerateEachCube
autoGenerateEachFace

CornerPoints.MeshGeneration. Testing in StlAutoGenerate
doesOpposingFaceExistInList
doesSameFaceExistInList
pushToList,
extractFaces
-}
