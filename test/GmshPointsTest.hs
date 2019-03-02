{-# LANGUAGE TemplateHaskell #-}
module GmshPointsTest(gmshPointsTestDo) where
{-
Test the GMSH.Points module.
-}

import CornerPoints.Points(Point(..))

import Test.HUnit

import qualified GMSH.Points as GP
import qualified GMSH.Common as GC
import qualified GMSH.Builder as GB
import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

gmshPointsTestDo = do
  --insert points without the Builder stack
  putStrLn("\n\nGmshPointsTest:")
  runTestTT insertPointTest
  runTestTT insertPointTest2
  runTestTT insertPointTest3
  runTestTT insertPointTest4
  --hash map tests
  runTestTT hashWithSaltPointTest
  runTestTT hashPointTest
  

--insert a hashed point, and dummy value into an emtpy map.
insertPointTest = TestCase $ assertEqual
  "insert a Point into an empty map"
  (GC.BuilderData HM.empty ( HM.fromList [(2171024669747360587,GC.PointsBuilderData 1 (Point 1 2 3))]) [1..] [1..])
  (GP.insert  [Point 1 2 3] $ GC.BuilderData HM.empty HM.empty [1..] [1..])

--Insert a Point into a map that already contains the point.
--As it is already in the map, map will not be modified, as indicated by the GP.UnChanged constructor.
insertPointTest2 = TestCase $ assertEqual
  "insert a Point into a map that already contains the point"
  (GC.BuilderData HM.empty (HM.fromList [(2171024669747360587,GC.PointsBuilderData 1 $ Point 1 2 3)]) [1..] [1..])
  ( let
      mapWithThePointAlreadyInserted = HM.insert (H.hash $ Point 1 2 3) (GC.PointsBuilderData 1 $ Point 1 2 3) HM.empty
      
    in
      GP.insert [Point 1 2 3] $ GC.BuilderData HM.empty mapWithThePointAlreadyInserted [1..] [1..]  
  )
--Insert a Point into a hash map that already contains a different point.
--The point will be inserted, as it does not already exist.
--The hashmap will be modified, as indicated by the GP.Changed constructor.
insertPointTest3 = TestCase $ assertEqual
  "insert a Point into a map that already contains a diff. point"
  (GC.BuilderData HM.empty (HM.fromList [(2171024669747360587,GC.PointsBuilderData 1 $ Point 1 2 3),
                                         (-8294074226866474165,GC.PointsBuilderData 2 $ Point 11 22 33)]) [1..] [1..] )
  ( let
      mapWithADiffPointAlreadyInserted = HM.insert  (H.hash $ Point 1 2 3) (GC.PointsBuilderData 1 (Point 1 2 3)) HM.empty
      
    in
      GP.insert [Point 11 22 33] $ GC.BuilderData HM.empty mapWithADiffPointAlreadyInserted [1..] [2..] 
  )


insertPointTest4 = TestCase $ assertEqual
  "insert a [Points], into an empty map"
  (GC.BuilderData HM.empty
                ( HM.fromList [(819944781536211787,GC.PointsBuilderData 3 $ Point 3 3 3),
                               (5911264160278557515,GC.PointsBuilderData 4 $ Point 4 4 4),
                               (-4271374597206133941,GC.PointsBuilderData 2 $ Point 2 2 2),
                               (3308183575658410827,GC.PointsBuilderData 1 $ Point 1 1 1)])
                  [1..] [1..]
  )
  (GP.insert  [Point 1 1 1, Point 2 2 2, Point 3 3 3, Point 4 4 4] $ GC.BuilderData HM.empty HM.empty [1..] [1..]) -- [1..] HM.empty)


------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------- test out using a hashmap--------------------------------------------------------

hashWithSaltPointTest = TestCase $ assertEqual
  "run hashWithSalt on a Point"
  (HM.fromList [(2171024669747360587,1)])
  (HM.insert (H.hashWithSalt 1 $ Point 1 2 3) 1 HM.empty)

hashPointTest = TestCase $ assertEqual
  "run hash on a Point"
  (HM.fromList [(2171024669747360587,1)])
  (HM.insert (H.hash $ Point 1 2 3) 1 HM.empty)
