module Stl.StlCornerPoints((|+++^|), (||+++^||), (+++^), Faces(..)) where

import CornerPoints.CornerPoints (CornerPoints(..), )
import Stl.StlBase (Triangle(..), newVertex)
import Control.Applicative

{- |
Convert [CornerPoints] to [Triangle] for writing out to an stl file.

Each CornerPoint will only show those faces as dictated by Faces. These will be
the faces of the CornerPoints which on the outer boundary of the total shape.
Eg: 2 cubes combined end to end, will hide the FrontFace of 1 cube, and the BackFace of the other cube, where they join.

Triangulation of faces:
Faces are made up of squares. To make sure that each triangle is within the boundary region of the square, including
squares with a reflex angle, the following triangulation is followed.
1st triangle: start at any vertex and follow the boundary in the required direction.
2nd triangle: start at the last vertex from 1st triangle, and continue around the boundary in required direction.

The inner/outer aspect of the face is determined by direction of triangulation, rather than using normals.
This varies, depending on the face in question. Eg: FrontFace vs BackFace. See each 'getTriangles' pattern match
for the particular implementation.
-}

{- ============================= to do =============================================
Any faces which have been reduced to a line, should not be shown.
EG: On a solid radial shape, the center is made up of BackFace, all of which are a single line.
Currently, used Faces constructor without a BackFace, fixes this. But it should be automatic,
and for all faces.
Regardless of whether a BackFace is used, Netfabb has no errors, but Slic3r has errors which it repairs without a problem.
-}

-- | Create triangles for a single CornerPoints
(+++^) :: Faces -> CornerPoints -> [Triangle]
face +++^ cornerPoints = getTriangles face cornerPoints

-- |Create triangles for a [CornerPoints], usually when dealing with a single layer.
(|+++^|) :: [Faces] -> [CornerPoints] -> [Triangle]
infix 3 |+++^|
faces |+++^| cornerPoints = concat $ zipWith (+++^) faces cornerPoints

-- |Create triangles for a [[CornerPoints]] such as all the CornerPoints used used to build up a shape.
(||+++^||) :: [[Faces]] -> [[CornerPoints]] -> [Triangle]
faces ||+++^|| cornerpoints = concat $ zipWith (|+++^|) faces cornerpoints
infix 3 ||+++^||
 

getTriangles :: Faces -> CornerPoints -> [Triangle]

getTriangles _ (CornerPointsError _) = []

getTriangles (FacesNada) c = []

getTriangles (FaceBack)  (CubePoints _ _ _ _ b1 b2 b3 b4) = 
 [
  (Triangle (newVertex b1) (newVertex b2) (newVertex b3)),
  (Triangle (newVertex b3) (newVertex b4) (newVertex b1))
 ]

getTriangles (FaceBottom)  (CubePoints f1 _ _ f4 b1 _ _ b4) = 
 [
  (Triangle (newVertex f1) (newVertex b1) (newVertex b4)),
  (Triangle (newVertex b4) (newVertex f4) (newVertex f1))
 ]

getTriangles (FaceFront)  (CubePoints f1 f2 f3 f4 _ _ _ _) = 
 [
  (Triangle (newVertex f1) (newVertex f4) (newVertex f3)),
  (Triangle (newVertex f3) (newVertex f2) (newVertex f1))
 ]

getTriangles (FaceLeft)  (CubePoints f1 f2 _ _ b1 b2 _ _) = 
 [
  (Triangle (newVertex f1) (newVertex f2) (newVertex b2)),
  (Triangle (newVertex b2) (newVertex b1) (newVertex f1))
 ]

getTriangles (FaceRight)  (CubePoints _ _ f3 f4 _ _ b3 b4) = 
 [
  (Triangle (newVertex f4) (newVertex b4) (newVertex b3)),
  (Triangle (newVertex b3) (newVertex f3) (newVertex f4))
 ] 

getTriangles (FaceTop)  (CubePoints _ f2 f3 _ _ b2 b3 _) = 
 [
  (Triangle (newVertex f2) (newVertex f3) (newVertex b3)),
  (Triangle (newVertex b3) (newVertex b2) (newVertex f2))
 ]

{-
try adding in other cube constructors for autogenerate.
This will allow a [FacesAll] to be |+++^| to [CornerPoints] even thougth they may be made up of Faces.

Should look at doing this in a better way.
-}


getTriangles (FacesAll) (BackFace b1 b2 b3 b4)   =
  getTriangles (FaceBack) (CubePoints b1 b2 b3 b4 b1 b2 b3 b4 )

getTriangles (FacesAll) (FrontFace f1 f2 f3 f4)   =
  getTriangles (FaceFront) (CubePoints f1 f2 f3 f4 f1 f2 f3 f4 )

getTriangles (FacesAll) (TopFace b2 f2 b3 f3)   =
  getTriangles (FaceTop) (CubePoints f2 f2 f3 f3 b2 b2 b3 b3 )
getTriangles (FacesAll) (BottomFace b1 f1 b4 f4)   =
  getTriangles (FaceBottom) (CubePoints f1 f1 f1 f4 b1 b1 b1 b4)

getTriangles (FacesAll) (LeftFace b1 b2 f1 f2)   =
  getTriangles (FaceLeft) (CubePoints f1 f2 f2 f2 b1 b2 b2 b2)

getTriangles (FacesAll) (RightFace b3 b4 f3 f4)   =
  getTriangles (FaceRight) (CubePoints f3 f3 f3 f4 b3 b3 b3 b4)
--end autogenerate

--this is the original that handled a full CubePoints
getTriangles (FacesAll) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceFront c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ]



getTriangles (FacesAllButBack) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceFront c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ] 

getTriangles (FacesAllButBottom) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceFront c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ] 

getTriangles (FacesAllButFront) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ]
 

getTriangles (FacesAllButLeft) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceFront c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesAllButRight) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceFront c),
  (getTriangles FaceLeft c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBackBottom) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c)
 ]


getTriangles (FacesBackBottomFront) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesBackBottomFrontLeft) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceFront c),
  (getTriangles FaceBack c),
  (getTriangles FaceBottom c)
 ]

getTriangles (FacesBackBottomFrontLeftTop) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceFront c),
  (getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBackBottomFrontRight) c = concat
 [(getTriangles FaceRight c),
  (getTriangles FaceFront c),
  (getTriangles FaceBack c),
  (getTriangles FaceBottom c)
 ]

getTriangles (FacesBackBottomFrontRightTop) c = concat
 [(getTriangles FaceRight c),
  (getTriangles FaceFront c),
  (getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBackBottomFrontTop) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceFront c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBackBottomLeft) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceBack c),
  (getTriangles FaceBottom c)
 ]

getTriangles (FacesBackBottomLeftRight) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceBack c),
  (getTriangles FaceBottom c)
 ]

getTriangles (FacesBackBottomLeftRightTop) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBackBottomTop) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBackFront) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceFront c)
 ] 

getTriangles (FacesBackFrontLeft) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceLeft c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesBackFrontLeftRight) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesBackFrontLeftRightTop) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceFront c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBackFrontLeftTop) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceLeft c),
  (getTriangles FaceFront c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBackFrontRight) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceRight c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesBackFrontTop) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceFront c),
  (getTriangles FaceTop c)
 ] 


getTriangles (FacesBackLeftRightTop) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ] 

getTriangles (FacesBackFrontRightTop) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceFront c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ] 

getTriangles (FacesBackTop) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBottomFront) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesBottomFrontLeft) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceLeft c),
  (getTriangles FaceFront c)
 ]


getTriangles (FacesBottomFrontLeftRightTop) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceFront c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBottomFrontTop) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceTop c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesBottomFrontRight) c = concat
 [(getTriangles FaceRight c),
  (getTriangles FaceFront c),
  (getTriangles FaceBottom c)
 ]

getTriangles (FacesBottomFrontLeftTop) c = concat
 [(getTriangles FaceTop c),
  (getTriangles FaceFront c),
  (getTriangles FaceLeft c),
  (getTriangles FaceBottom c)
 ]

 
getTriangles (FacesBottomFrontRightTop) c = concat
 [(getTriangles FaceTop c),
  (getTriangles FaceFront c),
  (getTriangles FaceRight c),
  (getTriangles FaceBottom c)
 ]

   
getTriangles (FacesBottomRightTop) c = concat
 [(getTriangles FaceRight c),
  (getTriangles FaceTop c),
  (getTriangles FaceBottom c)
 ]

getTriangles (FacesBottomLeft) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceLeft c)
 ]

getTriangles (FacesBottomLeftRight) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c)
 ]

getTriangles (FacesBottomLeftRightTop) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ]


getTriangles (FacesBottomFrontLeftRight) c = concat
 [(getTriangles FaceBottom c),
 (getTriangles FaceFront c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c)
 ]

getTriangles (FacesBottomTop) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesFrontLeftRightTop) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesFrontLeft) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesFrontLeftTop) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceTop c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesFrontRight) c = concat
 [(getTriangles FaceFront c),
  (getTriangles FaceRight c)
 ]

getTriangles (FacesFrontTop) c = concat
 [(getTriangles FaceTop c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesFrontRightTop) c = concat
 [(getTriangles FaceTop c),
  (getTriangles FaceFront c),
  (getTriangles FaceRight c)
 ]

getTriangles (FacesLeftRightTop) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesLeftTop) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceTop c)
 ]



------------------------------------------ Faces ----------------------------------------
{- |
Known uses: 
zip together with a [CornerPoints] to output stl triangles
-}
data Faces =
   FacesNada
 | FacesAll 
 | FacesAllButBack
 | FacesAllButBottom
 | FacesAllButFront
 | FacesAllButLeft
 | FacesAllButRight

 | FacesBackBottom
   
 | FacesBackBottomFront
 | FacesBackBottomFrontLeft
 | FacesBackBottomFrontLeftTop
 | FacesBackBottomFrontRight
 | FacesBackBottomFrontRightTop
 | FacesBackBottomFrontTop
 | FacesBackBottomLeft
 | FacesBackBottomLeftRight
 | FacesBackBottomLeftRightTop
 | FacesBackBottomTop
 | FacesBackFrontLeft
 | FacesBackFrontRight
 | FacesBackFront
 | FacesBackFrontLeftRight
 | FacesBackFrontLeftRightTop
 | FacesBackFrontLeftTop
 | FacesBackFrontTop
 | FacesBackTop
 | FacesBottomFront
 | FacesBackFrontRightTop
 | FacesBottomTop
 | FacesBottomFrontLeft
 | FacesBottomFrontLeftRight
 | FacesBottomFrontLeftTop
 | FacesBottomFrontLeftRightTop
 | FacesBottomFrontRight
 | FacesBottomFrontRightTop
 | FacesBottomFrontTop
 | FacesBottomLeft
 | FacesBottomLeftRight 
 | FacesBottomRightTop
 | FacesBackLeftRightTop
 | FacesFrontLeftTop
 | FacesFrontLeft
 | FacesFrontLeftRightTop 
 | FacesFrontRightTop
 | FacesFrontRight
 | FacesFrontTop
 | FacesLeftRight
 | FacesLeftTop
 | FacesLeftRightTop
 | FacesBottomLeftRightTop 
 | FaceBack
 | FaceBottom
 | FaceFront
 | FaceLeft
 | FaceRight
 | FaceTop
 
