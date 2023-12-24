module ParseJuicyTest (parseJuicyTestDo) where
import Test.HUnit
import Scan.ParseJuicy( getThePixelsRightOfCenter, convertPixelsToMillmeters, calculateRadiusFrom, averageValueOf,
                       removeLeftOfCenterPixels, TargetValueIndex(..), calculatePixelsPerMillmeter )
import CornerPoints.Radius(Radius(..))
import  Helpers.DSL (ofThe, forThe, andThen, adjustedFor, andThe,)
import qualified Data.Map as Map

{-
This is part of the orignal radial scanner which was not moved over from Tricad yet. Do I still need it?
-}

parseJuicyTestDo = do
  putStrLn "" 
  putStrLn "ParseJuicyTest"
  runTestTT calculateRadiusFromPixelsRightOfCenterTest
  
  runTestTT calculateMillimetersTest
  --get pixels right of center
  runTestTT adjustPixelIndexForLeftSlopeTestRedo
  runTestTT calculatePixelsPerMillmeterTest

  --get rid of blank values
  runTestTT whatIsDivdeZeroBy
  runTestTT averageValueOfValidListTest
  runTestTT averageValueOfEmptyListTest

  
  --runTestTT filePathBuilderBaseAllGoodTest
  --runTestTT filePathBuilderBaseNoSlashTest

  


{-
fileNameBuilder and filePathBuilderBase do not need to be exported, and
have been moved into a let clause.
Keep for testing them if required.

filePathBuilderBaseAllGoodTest = TestCase $ assertEqual
  "filePathBuilderBaseAllGoodTest"
  (Map.fromList [(0,"myPath/myFileName1010.JPG"),
                 (10,"myPath/myFileName1011.JPG")
                ])
  (filePathBuilderBase "myPath/" fileNameBuilder "myFileName" "JPG" [1010..] [0,10]  )

filePathBuilderBaseNoSlashTest = TestCase $ assertEqual
  "filePathBuilderBaseNoSlashTest"
  (Map.fromList [(0,"myPath/myFileName1010.JPG"),
                 (10,"myPath/myFileName1011.JPG")
                ])
  (filePathBuilderBase "myPath" fileNameBuilder "myFileName" "JPG" [1010..] [0,10]  )
-}


calculateRadiusFromPixelsRightOfCenterTest = TestCase $ assertEqual
  ("calculate Radius From Pixels Right Of Center Test")
  (Radius 4)
  (let pixelsPerMillemter = 100/1
       pixelsRightOfCenter = 200
       cameraAngle = 30
   in  calculateRadiusFrom  pixelsRightOfCenter (adjustedFor pixelsPerMillemter) $ andThe cameraAngle
       
  )

calculatePixelsPerMillmeterTest =  TestCase $ assertEqual
 "calculatePixelsPerMillmeterTest"
 (18.14220374220374)
 (calculatePixelsPerMillmeter     2592               390            37                      101 )
 --calculatePixelsPerMillmeter :: NumberOfPixels ->  Millimeters -> Millimeters          -> Millimeters          -> PixelsPerMillimeter
 --calculatePixelsPerMillmeter    imageWidthPx       imageWidthMM   objectWidthReadWorldMM  objectWidthOnScreenMM =
  
calculateMillimetersTest = TestCase $ assertEqual
  ("calculate millimeters from pixels")
  (2)
  (let pixelsPerMillemter = 50/1
   in  convertPixelsToMillmeters pixelsPerMillemter 100 
  )
  

{-
Given a center for top and bottom of image, and an index of the target value,
calculate the radius of the target value, adjusting for the slope of the laser line.
-}
adjustPixelIndexForLeftSlopeTestRedo = TestCase $ assertEqual
  ("adjust Pixel Index For Left Slope Test")
  ([10.0::TargetValueIndex, 20.0::TargetValueIndex, 30.0::TargetValueIndex])
  (let totalRows = 100
       topCenter = 110
       btmCenter = 90
       topRow = 0
       middleRow = 50
       btmRow = totalRows
       targetValueIndex = 120
       pixelsRightOfCenter = getThePixelsRightOfCenter $ andThen  (removeLeftOfCenterPixels btmCenter topCenter totalRows)
       
   in
       [
        pixelsRightOfCenter (forThe targetValueIndex) $ ofThe topRow,
        pixelsRightOfCenter (forThe targetValueIndex) $ ofThe middleRow,
        pixelsRightOfCenter (forThe targetValueIndex) $ ofThe btmRow 
       ]
  )

whatIsDivdeZeroBy  = TestCase $ assertEqual
  "whatIsDivdeZeroBy"
  0
  (0/3)

averageValueOfValidListTest = TestCase $ assertEqual
  "averageValueOfValidListTest"
  1.5
  (averageValueOf [1,2])


averageValueOfEmptyListTest = TestCase $ assertEqual
  "averageValueOfEmptyListTest"
  True
  (isNaN $ averageValueOf [])
