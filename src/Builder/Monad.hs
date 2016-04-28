{-# LANGUAGE TemplateHaskell #-}
module Builder.Monad() where
{-
ToDo:
Move this all over to Builder.hs, as it did not work out as a monad.

Create some tests.
-}

import CornerPoints.CornerPoints((|@+++#@|), (|+++|), CornerPoints(..), (+++), (+++>))
import CornerPoints.Points(Point(..))
import CornerPoints.Transpose (transposeZ, transposeY)
import CornerPoints.FaceConversions(upperFaceFromLowerFace, backFaceFromFrontFace )
import CornerPoints.FaceExtraction(extractFrontFace, extractTopFace)

import Control.Lens

import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)


import Test.HUnit hiding (State)

--have not used this yet. Might not need it.
newtype CornerPointsList = CornerPointsList [CornerPoints]

data Builder a = Builder {_cornerpoints :: a, _triangles :: [Triangle]}
makeLenses ''Builder
{-
instance (Eq) (Builder a) where
  Builder xs' triangles' == Builder xs'' triangles''
    | xs' == xs'' = True
    | otherwise = False
-}              

instance Functor Builder where
  fmap fx (Builder cornerpoints' triangles') = Builder (fx cornerpoints') triangles'

instance Applicative Builder where
  pure a = Builder a []
  --warning: No explicit implementation for ‘<*>’


instance Monad Builder  where
  return a = Builder a []
  (Builder cornerpoints' triangles') >>= fx = let (Builder cornerpoints'' triangles'')  = fx cornerpoints'
                                              in
                                                  Builder cornerpoints'' (triangles' ++ triangles'' )


builderTest = do
  let btmFace = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0 )
      cube = btmFace +++ (upperFaceFromLowerFace $ transposeZ (+1) btmFace )
      
  let inlineTest = TestCase $ assertEqual
        "inlineTest"
        ([CubePoints {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}, f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0}, f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}, b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0}, b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}, b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}}]
        ) 
        ( let cubeBuilder = 
               return [cube] >>=
                 (\cubes -> Builder cubes ( [FacesAll] |+++^| cubes ) )
                 
          in  cubeBuilder^.cornerpoints

              
        )
  runTestTT inlineTest

  let doTrianglesTest = TestCase $ assertEqual
        "doTest"
        ([CubePoints {f1 = Point {x_axis = 0.0, y_axis = 2.0, z_axis = 0.0}, f2 = Point {x_axis = 0.0, y_axis = 2.0, z_axis = 1.0}, f3 = Point {x_axis = 1.0, y_axis = 2.0, z_axis = 1.0}, f4 = Point {x_axis = 1.0, y_axis = 2.0, z_axis = 0.0}, b1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}, b2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0}, b3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}, b4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}}]
        ) 
        ( let cubeBuilder :: Builder [CornerPoints]
              cubeBuilder = do
                builder1 <- Builder [cube] ( [FacesAll] |+++^| [cube] )
                builder2 <- (let lclCubes = builder1
                                         |+++|
                                         (map ((transposeY (+1)) . extractFrontFace)  builder1)
                                           
                          in  Builder lclCubes ([FacesAll] |+++^| lclCubes) 
                         )
                return builder2
                 
                 
          in  cubeBuilder^.cornerpoints
        )
  runTestTT doTrianglesTest


lookAtStlShape =
  let btmFace = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0 )
      cube = btmFace +++ (upperFaceFromLowerFace $ transposeZ (+1) btmFace )
      cubeBuilder :: Builder [CornerPoints]
      cubeBuilder = do
                builder1 <- Builder [cube] ( [FacesBackBottomLeftRightTop] |+++^| [cube] )
                builder2 <- (let lclCubes = builder1
                                         |+++|
                                         (map ((transposeY (+1)) . extractFrontFace)  builder1)
                                           
                          in  Builder lclCubes ([FacesBottomFrontLeftRight] |+++^| lclCubes) 
                         )
                builder3 <-
                  (let lclCubes = builder2
                                  |+++|
                                  (map ((transposeZ (+1)) . extractTopFace)  builder2)
                   in  Builder lclCubes ([FacesBackFrontLeftRightTop] |+++^| lclCubes)
                  )
                return builder3
  in
    writeStlToFile $ newStlShape "builder monad test" (cubeBuilder^.triangles)
