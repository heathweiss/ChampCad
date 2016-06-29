module CornerPointsWithDegreesTest(cornerPointsWithDegreesTest) where
import Test.HUnit
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (+++~), (+++~>), (|+++~|), (@~+++#@), DegreeRange(..), cornerPointsWithDegreesWithinRange )
import CornerPoints.CornerPoints(CornerPoints(..),(@+++#@),(+++), (+++>))
import CornerPoints.Points (Point(..))
import CornerPoints.Transpose (transposeZ)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import  CornerPoints.FaceExtraction (
 extractFrontFace,
 extractBottomFrontLine,
 extractFrontTopLine,
 extractTopFace,
 extractBackTopLine,
 extractBottomFace,
 extractBackBottomLine,
 extractBackFace
 )

testCube = (BottomFace
              {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0},
               b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}}
           )
           @+++#@
           ((transposeZ(+1)) . upperFaceFromLowerFace)

testRightFace = 
           (RightFace (Point 1 0 1) (Point 1 0 0) (Point 1 1 1) (Point 1 1 0))

testLeftFace = 
           (LeftFace  (Point 0 0 0)  (Point 0 0 1)  (Point 0 1 0) (Point 0 1 1) )
           
cornerPointsWithDegreesTest = do

  {-Try out the (@~+++#@) by adding a frontFace to a Cube-}
  let transposeACornerPointsWithDegreesUpwards = TestCase $ assertEqual
       "transposeACornerPointsWithDegreesUpwards"
       (CubesWithStartEndDegrees
        {_cube = CubePoints {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
                             f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 2.0},
                             f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 2.0},
                             f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0},
                             b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
                             b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 2.0},
                             b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 2.0},
                             b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}}, _degreeRange = DegreeRange 0.0 10.0})
       ( (CubesWithStartEndDegrees testCube (DegreeRange 0 10)) @~+++#@ ((transposeZ (+1)). extractTopFace)
       
       )
  runTestTT transposeACornerPointsWithDegreesUpwards


  {-Push a RightFaceWithDegrees into a [LeftFaceWithDegrees]-}
  let pushRightFaceWithDegreesIntoAListOfLeftFaceWithDegrees = TestCase $ assertEqual
        "pushRightFaceWithDegreesIntoAListOfLeftFaceWithDegrees"
        [CubesWithStartEndDegrees
           {_cube = CubePoints
                      {f1 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0},
                       f2 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0},
                       f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0},
                       f4 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}, 
                       b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
                       b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                       b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                       b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}
                      },
              _degreeRange = (DegreeRange 0.0 10.0)},
         CubesWithStartEndDegrees 
           {_cube = CubePoints {f1 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 8.0},
                       f2 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0},
                       f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0},
                       f4 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0},
                       b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0},
                       b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
                       b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                       b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}},
              _degreeRange = (DegreeRange 10.0 20.0)}
        ]
        (let listOfLeftFacesWithDegrees =
                [LeftFaceWithDegrees
                  {_leftFace = LeftFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
                                         f2 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0}, f1 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}},
                  _degreeRange = (DegreeRange 10.0 10.0)},
                 LeftFaceWithDegrees
                  {_leftFace = LeftFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0},
                              f2 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}, f1 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 8.0}},
                 _degreeRange = (DegreeRange 20.0 20.0)}
                ]
  
             rightFaceWithDegrees =
                RightFaceWithDegrees
                  {_rightFace = RightFace {b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
                                           f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0}, f4 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}},
                   _degreeRange = (DegreeRange 0.0 0.0)}
  
         in rightFaceWithDegrees +++~> listOfLeftFacesWithDegrees
           
        )
  runTestTT pushRightFaceWithDegreesIntoAListOfLeftFaceWithDegrees
  
  
  
  
  
    
  {-Create a [CubesWithStartEndDegrees] from FaceWithDegrees, then filter them-}
  let createCubesWithStartEndDegreesFromFaceWithDegrees = TestCase $ assertEqual
        "createCubesWithStartEndDegreesFromFaceWithDegrees"
        [
           CubesWithStartEndDegrees testCube (DegreeRange 90 180) ,
           CubesWithStartEndDegrees testCube (DegreeRange 180 270)
        ]
        (cornerPointsWithDegreesWithinRange (DegreeRange 90 270)
         [
           (RightFaceWithDegrees testRightFace (DegreeRange 0 0)) +++~ (LeftFaceWithDegrees testLeftFace (DegreeRange 90 90)),
           (RightFaceWithDegrees testRightFace (DegreeRange 90 90)) +++~ (LeftFaceWithDegrees testLeftFace (DegreeRange 180 180)),
           (RightFaceWithDegrees testRightFace (DegreeRange 180 180)) +++~ (LeftFaceWithDegrees testLeftFace (DegreeRange 270 270)),
           (RightFaceWithDegrees testRightFace (DegreeRange 270 270)) +++~ (LeftFaceWithDegrees testLeftFace (DegreeRange 360 360))
         ]
        )
  runTestTT createCubesWithStartEndDegreesFromFaceWithDegrees
  

  

  
  {-
  Create a map of CubesWithStartEndDegrees.
  See if it is in range.
  Test a 
  -}
  let  
       cubesWithStartEndDegreesTest = TestCase $ assertEqual
        "get range of CubesWithStartEndDegrees, all but 1st "
        [
           CubesWithStartEndDegrees testCube (DegreeRange 90 180) ,
           CubesWithStartEndDegrees testCube (DegreeRange 180 270),
           CubesWithStartEndDegrees testCube (DegreeRange 270 360)
        ]
        (cornerPointsWithDegreesWithinRange (DegreeRange 90 360)
         [
           CubesWithStartEndDegrees testCube (DegreeRange 0 90) ,
           CubesWithStartEndDegrees testCube (DegreeRange 90 180) ,
           CubesWithStartEndDegrees testCube (DegreeRange 180 270) ,
           CubesWithStartEndDegrees testCube (DegreeRange 270 360) 
         ]
        )
  runTestTT cubesWithStartEndDegreesTest


  let  
       cubesWithStartEndDegreesTest2 = TestCase $ assertEqual
        "get range of CubesWithStartEndDegrees, all but last "
        [  CubesWithStartEndDegrees testCube (DegreeRange 0 90),
           CubesWithStartEndDegrees testCube (DegreeRange 90 180) ,
           CubesWithStartEndDegrees testCube (DegreeRange 180 270)
           
        ]
        (cornerPointsWithDegreesWithinRange (DegreeRange 0 270)
         [
           CubesWithStartEndDegrees testCube (DegreeRange 0 90) ,
           CubesWithStartEndDegrees testCube (DegreeRange 90 180) ,
           CubesWithStartEndDegrees testCube (DegreeRange 180 270) ,
           CubesWithStartEndDegrees testCube (DegreeRange 270 360) 
         ]
        )
  runTestTT cubesWithStartEndDegreesTest2

  let  
       cubesWithStartEndDegreesTest3 = TestCase $ assertEqual
        "get range of CubesWithStartEndDegrees, all"
        [  CubesWithStartEndDegrees testCube (DegreeRange 0 90),
           CubesWithStartEndDegrees testCube (DegreeRange 90 180) ,
           CubesWithStartEndDegrees testCube (DegreeRange 180 270),
           CubesWithStartEndDegrees testCube (DegreeRange 270 360)
        ]
        (cornerPointsWithDegreesWithinRange (DegreeRange 0 360)
         [
           CubesWithStartEndDegrees testCube (DegreeRange 0 90) ,
           CubesWithStartEndDegrees testCube (DegreeRange 90 180) ,
           CubesWithStartEndDegrees testCube (DegreeRange 180 270) ,
           CubesWithStartEndDegrees testCube (DegreeRange 270 360) 
         ]
        )
  runTestTT cubesWithStartEndDegreesTest3

  let addLeftRigtFacesTest = TestCase $ assertEqual
        "addLeftRigtFacesTest"
        (CubesWithStartEndDegrees (testRightFace +++ testLeftFace) (DegreeRange 0 90))
        (
           (RightFaceWithDegrees testRightFace (DegreeRange 0 0)) +++~ (LeftFaceWithDegrees testLeftFace (DegreeRange 90 90))
        )
  runTestTT addLeftRigtFacesTest


  {- degrees360Tuples has been moved inside of newCornerPointsWithDegreesList. Leave here in case further testing is needed.
  let seeDegrees = TestCase $ assertEqual
        "seeDegrees"
        [(0,10),(10,20),(20,30),(30,40),(40,50),(50,60),(60,70),(70,80),(80,90),(90,100),(100,110),(110,120),(120,130),(130,140),(140,150),(150,160),(160,170),
         (170,180),(180,190),(190,200),(200,210),(210,220),(220,230),(230,240),(240,250),(250,260),(260,270),(270,280),(280,290),(290,300),(300,310),(310,320),
         (320,330),(330,340),(340,350),(350,360)]
        degrees360Tuples
  runTestTT seeDegrees
  -}
