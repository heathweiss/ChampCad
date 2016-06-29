{-# LANGUAGE OverloadedStrings #-}
module JsonTest(jsonTestDo) where
import Test.HUnit
import CornerPoints.Radius(SingleDegreeRadii(..), Radius(..), MultiDegreeRadii(..))
import Scan.Json
import Data.Aeson
import qualified Data.ByteString.Lazy as BS


jsonTestDo = do
  runTestTT radiusDecodeTest
  runTestTT radiusEncodeTest
  runTestTT degreeEncodeTest
  runTestTT degreeDecodeTest
  runTestTT scanEncodeTest
  runTestTT scanDecodeTest

---------------------------------scan----------------------------------------------
scanEncodeTest = TestCase $ assertEqual
  "encode scan"
  ( "{\"degrees\":[{\"radii\":[{\"radius\":12},{\"radius\":120}],\"degree\":0.0},{\"radii\":[{\"radius\":120},{\"radius\":1200}],\"degree\":10}],\"name\":\"myScan\"}" )

  (encode (MultiDegreeRadii
               { name = "myScan",
                 degrees =
                  [
                   SingleDegreeRadii {degree=0, radii=[Radius {radius=12}, Radius {radius=120}]},
                   SingleDegreeRadii {degree=10, radii=[Radius {radius=120}, Radius {radius=1200}]}
                  ]
               }
          )
  )



scanDecodeTest = TestCase $ assertEqual
  "decode scan"
  (Just
       (MultiDegreeRadii
               { name = "myScan",
                 degrees =
                  [
                   SingleDegreeRadii {degree=0, radii=[Radius {radius=12}, Radius {radius=120}]},
                   SingleDegreeRadii {degree=10, radii=[Radius {radius=120}, Radius {radius=1200}]}
                  ]
               }
          )
  )
  ((decode  "{\"degrees\":[{\"radii\":[{\"radius\":12},{\"radius\":120}],\"degree\":0},{\"radii\":[{\"radius\":120},{\"radius\":1200}],\"degree\":10}],\"name\":\"myScan\"}"     ) :: Maybe MultiDegreeRadii)

------------------------- degree -------------------------------------------------
degreeEncodeTest = TestCase $ assertEqual
  "encode radius"
  ("{\"radii\":[{\"radius\":12},{\"radius\":120}],\"degree\":0.0}")
  (encode (SingleDegreeRadii {degree=0, radii=[Radius {radius=12}, Radius {radius=120}]}))

degreeDecodeTest = TestCase $ assertEqual
  "decode degree"
  (Just (SingleDegreeRadii 0 [Radius 12, Radius 120]))
  ((decode  "{\"radii\":[{\"radius\":12},{\"radius\":120}],\"degree\":0}") :: Maybe SingleDegreeRadii)





-------------------------- radius ----------------------------------
radiusDecodeTest = TestCase $ assertEqual
  "decode radius"
  (Just (Radius 12))
  ((decode  "{\"radius\":12}") :: Maybe Radius)


radiusEncodeTest = TestCase $ assertEqual
  "encode a radius"
  ( ( "{\"radius\":12}"))
  (encode $  (Radius {radius=12}) )
