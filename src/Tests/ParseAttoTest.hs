module Tests.ParseAttoTest(parseAttoTestDo) where
import Test.HUnit
import Scan.ParseAtto(SingleDegreePixelValues(..), MultiDegreePixelValues(..), parseCSVPixelValues)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Data.Char
import Data.Word
import Control.Applicative
import qualified CornerPoints.Create as MP (Radius(..), MultiDegreeRadii(..), SingleDegreeRadii(..))
import qualified  Data.ByteString.Internal as BI (unpackBytes)
import qualified  Data.ByteString.Char8 as BC (pack) 
import GHC.Word (Word8)
import Scan.Transform(pixelIndicesOfPixelValuesLTE, pixelIndicesAverageToRadius, reduceRows, reduceScanRows, multiDegreePixelValuesToMultiDegreeRadii)

--create a [Word8] for: Right(B.pack $ strToWord8s)
--which gets a bytstring of word8.
--This is how to create fake data, that corresponds to csv pixel values read in from a file.
strToWord8s :: String -> [Word8]
strToWord8s = BI.unpackBytes . BC.pack

parseAttoTestDo = do
  

  --degrees
  
  runTestTT getACompleteRawScan
  runTestTT getAScanFromARawScan
  runTestTT reduceScanRowsTest

getACompleteRawScan = TestCase $ assertEqual
  "get a complete scan"
  (Right(MultiDegreePixelValues {degrees=[(SingleDegreePixelValues {degree=1, radii= [[10.0,20.0],[10.0,20.0]]}),
                              (SingleDegreePixelValues {degree=2, radii= [[1.0,2.0],[3.0,4.0]]})
                             ]}))
  (Right (B.pack $strToWord8s "1 10 20;10 20$2 1 2;3 4")  >>=  parseCSVPixelValues)

getAScanFromARawScan = TestCase $ assertEqual
  "get a Scan from a RawScan"
  (Right(MP.MultiDegreeRadii { MP.name="myScan",
                            MP.degrees=[(MP.SingleDegreeRadii {MP.degree=1, MP.radii= [MP.Radius 0.5,MP.Radius 0.5]}),
                            (MP.SingleDegreeRadii {MP.degree=2, MP.radii= [MP.Radius 0.5,MP.Radius 0.0]})
                             ]}))
  ( let rawScan = (Right (B.pack $strToWord8s "1 1 2 3;1 2 3$2 1 2 3;1 3 3")  >>=  parseCSVPixelValues)
    in  multiDegreePixelValuesToMultiDegreeRadii  "myScan" (pixelIndicesAverageToRadius . pixelIndicesOfPixelValuesLTE 2) rawScan
  )


{-
create a 2 row raw scan the same as from my scan raw project to see what is going wrong.
-}
reduceScanRowsTest = TestCase $ assertEqual
  "get a Scan from a RawScan and use ReduceScanRows on it"
  
  (Right (MP.MultiDegreeRadii {MP.name = "myScan", MP.degrees = [MP.SingleDegreeRadii {MP.degree = 0.0, MP.radii = [MP.Radius {MP.radius = 0.5}]},MP.SingleDegreeRadii {MP.degree = 90.0, MP.radii = [MP.Radius {MP.radius = 0.5}]},MP.SingleDegreeRadii {MP.degree = 180.0, MP.radii = [MP.Radius {MP.radius = 0.5}]},MP.SingleDegreeRadii {MP.degree = 270.0, MP.radii = [MP.Radius {MP.radius = 0.5}]},MP.SingleDegreeRadii {MP.degree = 360.0, MP.radii = [MP.Radius {MP.radius = 0.5}]}]}))
  
  ( let rawScan = (Right (B.pack $strToWord8s "0 1 2 3;1 2 3;1 2 3$90 1 2 3;1 2 3;1 2 3$180 1 2 3;1 2 3;1 2 3$270 1 2 3;1 2 3;1 2 3$360 1 2 3;1 2 3;1 2 3")  >>=  parseCSVPixelValues)
    in  multiDegreePixelValuesToMultiDegreeRadii "myScan" (pixelIndicesAverageToRadius . pixelIndicesOfPixelValuesLTE 2) rawScan  >>= reduceScanRows 2
    
  )


{-
Scan.ParseAtto no longer exports these functions.
Leave this testing in place in case need further testing at a later point.
--radius
  runTestTT readARowOfInts
  runTestTT readMultiRowsOfInts
  runTestTT readRowOfIntsAndReducetoAverageMinVals
  runTestTT readMultiRowsOfIntsAndReduceToAverageMinVals
  runTestTT readMultiRowsOfIntsReduceRowsAndReduceToAverageMinVals
--degrees
  runTestTT getADegree
  runTestTT getADegreeThenARowOfInts

readARowOfInts = TestCase $ assertEqual
  "read a list of ints"
  (Right[10,20, 30,40])
  ((Right (B.pack $strToWord8s "10 20 30 40;50")  >>=  parseOnly  getPixelRow))


readMultiRowsOfInts = TestCase $ assertEqual
  "read a multi lines of ints"
  (Right[[10,20, 30,40],[50, 60]])
  ((Right (B.pack $strToWord8s "10 20 30 40;50 60")  >>=  parseOnly  getPixelRowMulti))

readRowOfIntsAndReducetoAverageMinVals = TestCase $ assertEqual
  "read a single row of Ints, and reduce them down to average index of min vals"
  (MP.Radius 1.0)--(1.0)
  ( let parseResults =  (Right(B.pack $ strToWord8s "10 20 30 40")  >>=  parseOnly  getPixelRow)
    in 
        case parseResults of
         Right ints -> pixelIndicesAverageToRadius $ pixelIndicesOfPixelValuesLTE 30 ints
        
  )

readMultiRowsOfIntsAndReduceToAverageMinVals = TestCase $ assertEqual
 "read multi rows of ints, and reduce them down to average index of min values."
 ([(MP.Radius 0.5),(MP.Radius 0.5)])
 ( let parseResults = (Right (B.pack $strToWord8s "10 20 30 40;10 20 30 50")  >>=  parseOnly  getPixelRowMulti)
       
   in  case parseResults of
        Right rowsOfInts -> map (pixelIndicesAverageToRadius . pixelIndicesOfPixelValuesLTE 20 ) rowsOfInts
 )


readMultiRowsOfIntsReduceRowsAndReduceToAverageMinVals = TestCase $ assertEqual
 "read multi rows of ints, reduce the # of rows, and then reduce them down to average index of min values."
 ([MP.Radius 0.5])
 ( let parseResults = (Right (B.pack $strToWord8s "10 20 30 40;10 20 30 50")  >>=  parseOnly  getPixelRowMulti)
       
   in  case parseResults of
        Right rowsOfInts -> map (pixelIndicesAverageToRadius . pixelIndicesOfPixelValuesLTE 20 ) $ reduceRows 2 rowsOfInts
 )

 
getADegree = TestCase $ assertEqual
  "remove the leading degree"
  (Right 1)
  (Right (B.pack $strToWord8s "1 10 20 30 40;10 20 30 50")  >>=  parseOnly  getDegree)
       

getADegreeThenARowOfInts = TestCase $ assertEqual
  "get a degree, then a row of ints"
  (Right (SingleDegreePixelValues {degree=1, radii= [[10.0,20.0],[10.0,20.0]]}) )
  (Right (B.pack $strToWord8s "1 10 20;10 20")  >>=  parseOnly  getRawDegreeScan)
-}
