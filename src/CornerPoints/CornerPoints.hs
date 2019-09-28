{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ParallelListComp #-}

module CornerPoints.CornerPoints(
CornerPoints(..),
(+++),
(===),
(|===|),
(++++),
(+++-),
(|+++|),
(+++>),
(@+++#@),
(&+++#@),
(|@+++#@|),
(##+++#),
(+++#),
scaleCornerPoints,
scaleCornerPointsZ,
CornerPointsBuilder(..),
cornerPointsError, findCornerPointsError,
isCubePoints, isCubePointsList,
getCornerPointsWithIndex,
--cpointType,
toPoints, toPointsFromList
) where

import Control.Lens

import Data.Data
import Data.Typeable

import CornerPoints.Points (Point(..))

import    Control.Applicative

import Data.List(find)
import qualified TypeClasses.Showable as TS


-- import Math.Distance(Distance(..),Distant, calculateDistance, DistanceA(..),DistantA, calculateDistanceA)

infix 7 +++
infix 6 +++-
infix 5 @+++#@ 
infix 5 +++>
infix 4 |+++|
--infix 3 +++^
data CornerPoints =
               CornerPointsError
        {
               errMessage :: String
        }
        |
               CornerPointsId
        |
               -- | Counts as a CubePoints in isCubePointsList and isCubePoints as used by Builder.Monad
               CornerPointsNothing
        |
               CubePoints 

        {       f1 :: Point,
                f2 :: Point,
                f3 :: Point,
                f4 :: Point,
                b1 :: Point,
                b2 :: Point,
                b3 :: Point,
                b4 :: Point
        }
        |
        -------------------------------------- Faces --------------------
        BackFace
        {       b1 :: Point,
                b2 :: Point,
                b3 :: Point,
                b4 :: Point
        }
        |
        BottomFace
        {       b1 :: Point,
                f1 :: Point,
                b4 :: Point,
                f4 :: Point
        }
        |
        FrontFace
        {       f1 :: Point,
                f2 :: Point,
                f3 :: Point,
                f4 :: Point
        }
        |
        LeftFace
        {       b1 :: Point,
                b2 :: Point,
                f1 :: Point,
                f2 :: Point
        }
        |
        RightFace
        {       b3 :: Point,
                b4 :: Point,
                f3 :: Point,
                f4 :: Point
        }
        |
        TopFace
        {       b2 :: Point,
                f2 :: Point,
                b3 :: Point,
                f3 :: Point
        }
        |
        
        

        ------------------------------ lines -------------------------


         BackBottomLine
        {
                b1 :: Point,
                b4 :: Point 
        }
        |
        BackTopLine
        {
                b2 :: Point,
                b3 :: Point
        }
        |
        BottomFrontLine
        {
                f1 :: Point,
                f4 :: Point
        }
        |
        BottomLeftLine
        {
                b1 :: Point,
                f1 :: Point
        }
        |
        BackRightLine
        {
                b3 :: Point,
                b4 :: Point
        }
        |
        BackLeftLine
        {
                b1 :: Point,
                b2 :: Point
        }
        |
        BottomRightLine
        {
                b4 :: Point,
                f4 :: Point
        }
        |
        FrontLeftLine
        {       f1 :: Point,
                f2 :: Point
        }
        |
        FrontRightLine
        {       f3 :: Point,
                f4 :: Point
        }
        |
        FrontTopLine
        {       f2 :: Point,
                f3 :: Point
        }
        |
        TopLeftLine
        {
                b2 :: Point,
                f2 :: Point
        }
        |
        TopRightLine
        {
                b3 :: Point,
                f3 :: Point
        }
        --------------------------------- points ------------------------------

        |
        B1
        {
                b1 :: Point
        }
        |
        B2
        {
                b2 :: Point
        }
        |
        B3
        {
                b3 :: Point
        }
        |
        B4
        {
                b4 :: Point
        }
        |
        F1
        {
                f1 :: Point
        }
        |
        F2
        {
                f2 :: Point
        }
        |
        F3
        {
                f3 :: Point
        }
        |
        F4
        {
                f4 :: Point
        }
        deriving (Show, Typeable, Data)


instance TS.Showable CornerPoints

{-
True if CornerPointsError otherwise false.
CornerPointsError is the CornerPoints constructor for an error message.
-}
cornerPointsError :: CornerPoints -> Bool
cornerPointsError (CornerPointsError _) = True
cornerPointsError _                     = False

-- | Use it with Data.List(find) to search a [CornerPoints] for a CornerPointsError.
findCornerPointsError :: [CornerPoints] -> Maybe CornerPoints
findCornerPointsError cornerPoints = find cornerPointsError cornerPoints

-- | True if CubePoints, otherwise false.
isCubePoints :: CornerPoints -> Bool
isCubePoints (CubePoints _ _ _ _ _ _ _ _) = True
isCubePoints CornerPointsNothing = True
isCubePoints _ = False



{- |
True if the [CornerPoints] only contains CubePoints, otherwise false.
-}
isCubePointsList :: [CornerPoints] -> Bool
isCubePointsList cpoints =
  let isNotCubePoints :: CornerPoints -> Bool
      isNotCubePoints (CubePoints _ _ _ _ _ _ _ _)  = False
      isNotCubePoints CornerPointsNothing = False
      isNotCubePoints _ = True
  in
   
  case find (isNotCubePoints) cpoints of
    Nothing -> True
    Just _  -> False



--------------------------------------------------- Equal-----------------------------------------------------------
{- |
Implement as part of Equal class.

Used for:
So that assertions can be made for testing.

Equal if:
They are both the same type of CornerPoint and each of the axis is equal

Not equal if: 
They are not the same constructor.
x y z axis are not all equal.

Need to be implemented for each constuctor

Due to rounding errors, etc. a special function to compare x y z axis values is required
to make sure they are withing 0.01 of each othere
-}

instance Eq CornerPoints where
    -------------------------- points -------------------
    B1 b1 == B1 b1a
       |  b1 == b1a = True
       | otherwise = False
  
    B2 b2 == B2 b2a
       |  b2 == b2a = True
       | otherwise = False

    B3 b3 == B3 b3a
       |  b3 == b3a = True
       | otherwise = False
  
    B4 b4 == B4 b4a
       |  b4 == b4a = True
       | otherwise = False

    F1 f1 == F1 f1a  
       | f1 == f1a = True 
       | otherwise = False

    F2 f2 == F2 f2a  
       | f2 == f2a = True 
       | otherwise = False 

    F3 f3  == F3 f3a  
      | f3 == f3a = True
      | otherwise = False

    F4 f4  == F4 f4a  
      | f4 == f4a = True
      | otherwise = False
    --------------------------- lines ----------------------
    BackBottomLine b1 b4 == BackBottomLine b1a b4a
      | (b1 == b1a) && (b4 == b4a) = True
      | otherwise = False


    BackTopLine b2 b3 == BackTopLine b2a b3a
      | (b2 == b2a) && (b3 == b3a) = True
      | otherwise = False

    BottomFrontLine f1 f4 == BottomFrontLine f1a f4a
      | (f1 == f1a) && (f4 == f4a) = True
      | otherwise = False

    BottomLeftLine b1 f1 == BottomLeftLine b1a f1a
      | (b1 == b1a) && (f1 == f1a) = True
      | otherwise = False

    BottomRightLine b4 f4  == BottomRightLine b4a f4a
      | (b4 == b4a) && (f4 == f4a) = True
      | otherwise = False

    FrontTopLine f2 f3 == FrontTopLine f2a f3a
      | (f2 == f2a) && (f3 == f3a) = True
      | otherwise = False

    TopLeftLine b2 f2 == TopLeftLine b2a f2a
      | (b2 == b2a) && (f2 == f2a) = True
      | otherwise = False

    TopRightLine b3 f3 == TopRightLine b3a f3a
      | (b3 == b3a) && (f3 == f3a) = True
      | otherwise = False

    FrontLeftLine f1 f2 == FrontLeftLine f1' f2'
      | (f1 == f1') && (f2 == f2') = True
      | otherwise = False

    ------------------------------- faces ---------------------------
    FrontFace f1 f2 f3 f4 == FrontFace f1a f2a f3a f4a
      | (f1 == f1a) && (f2 == f2a) && (f3 == f3a) && (f4 == f4a) = True
      | otherwise = False
    
    BottomFace b1 f1 b4 f4 == BottomFace b1a f1a b4a f4a
      | (b1 == b1a) && (f1 == f1a) && (b4 == b4a) && (f4 == f4a) = True
      | otherwise = False
    
    TopFace b2 f2 b3 f3 == TopFace b2a f2a b3a f3a
      | (b2 == b2a) && (f2 == f2a) && (b3 == b3a) && (f3 == f3a) = True
      | otherwise = False

    RightFace b3 b4 f3 f4 == RightFace b3a b4a f3a f4a
      | (b3 == b3a) && (b4 == b4a) && (f3 == f3a) && (f4 == f4a) = True
      | otherwise = False

    LeftFace b1 b2 f1 f2 == LeftFace b1a b2a f1a f2a
      | (b1 == b1a) && (b2 == b2a) && (f1 == f1a) && (f2 == f2a) = True
      | otherwise = False

    BackFace b1 b2 b3 b4 == BackFace b1a b2a b3a b4a
      | (b1 == b1a) && (b2 == b2a) && (b3 == b3a) && (b4 == b4a)  = True
      | otherwise = False
    ---------------------------------- cubes --------------------
    CubePoints f1 f2 f3 f4 b1 b2 b3 b4 == CubePoints f1a f2a f3a f4a b1a b2a b3a b4a
      | (f1 == f1a) && (f2 == f2a) && (f3 == f3a) && (f4 == f4a) && (b1 == b1a) && ( b2 == b2a) && (b3 == b3a) && (b4 == b4a) = True
      | otherwise = False

    CornerPointsError errMessage' == CornerPointsError errMessage'' =
      errMessage' == errMessage''

    CornerPointsNothing == CornerPointsNothing = True
    CornerPointsNothing == _ = False
    _ == CornerPointsNothing = False

    anyThingElse == isFalseOrNeedsAPatterMatch = False

--------------------------------------------------- Equal ===-----------------------------------------------------------
{- |
An Either String Bool version of ==
-}
(===) :: CornerPoints -> CornerPoints -> Either String Bool


-------------------------- points -------------------
B1 b1 === B1 b1a
       |  b1 == b1a = Right True
       | otherwise = Right False
  
B2 b2 === B2 b2a
       |  b2 == b2a = Right True
       | otherwise = Right False

B3 b3 === B3 b3a
       |  b3 == b3a = Right True
       | otherwise = Right False
  
B4 b4 === B4 b4a
       |  b4 == b4a = Right True
       | otherwise = Right False

F1 f1 === F1 f1a  
       | f1 == f1a = Right True 
       | otherwise = Right False

F2 f2 === F2 f2a  
       | f2 == f2a = Right True 
       | otherwise = Right False 

F3 f3  === F3 f3a  
      | f3 == f3a = Right True
      | otherwise = Right False

F4 f4  === F4 f4a  
      | f4 == f4a = Right True
      | otherwise = Right False
    --------------------------- lines ----------------------
BackBottomLine b1 b4 === BackBottomLine b1a b4a
      | (b1 == b1a) && (b4 == b4a) = Right True
      | otherwise = Right False


BackTopLine b2 b3 === BackTopLine b2a b3a
      | (b2 == b2a) && (b3 == b3a) = Right True
      | otherwise = Right False

BottomFrontLine f1 f4 === BottomFrontLine f1a f4a
      | (f1 == f1a) && (f4 == f4a) = Right True
      | otherwise = Right False

BottomLeftLine b1 f1 === BottomLeftLine b1a f1a
      | (b1 == b1a) && (f1 == f1a) = Right True
      | otherwise = Right False

BottomRightLine b4 f4  === BottomRightLine b4a f4a
      | (b4 == b4a) && (f4 == f4a) = Right True
      | otherwise = Right False

FrontTopLine f2 f3 === FrontTopLine f2a f3a
      | (f2 == f2a) && (f3 == f3a) = Right True
      | otherwise = Right False

TopLeftLine b2 f2 === TopLeftLine b2a f2a
      | (b2 == b2a) && (f2 == f2a) = Right True
      | otherwise = Right False

TopRightLine b3 f3 === TopRightLine b3a f3a
      | (b3 == b3a) && (f3 == f3a) = Right True
      | otherwise = Right False

FrontLeftLine f1 f2 === FrontLeftLine f1' f2'
      | (f1 == f1') && (f2 == f2') = Right True
      | otherwise = Right False

FrontRightLine f3 f4 === FrontRightLine f3' f4'
      | (f3 == f3') && (f4 == f4') = Right True
      | otherwise = Right False
      
BackLeftLine b1 b2 === BackLeftLine b1' b2'
      | (b1 == b1') && (b2 == b2') = Right True
      | otherwise = Right False
      
BackRightLine b2 b3 === BackRightLine b2' b3'
      | (b2 == b2') && (b3 == b3') = Right True
      | otherwise = Right False
    ------------------------------- faces ---------------------------
FrontFace f1 f2 f3 f4 === FrontFace f1a f2a f3a f4a
      | (f1 == f1a) && (f2 == f2a) && (f3 == f3a) && (f4 == f4a) = Right True
      | otherwise = Left "frontface temp error" -- Right False
    
BottomFace b1 f1 b4 f4 === BottomFace b1a f1a b4a f4a
      | (b1 == b1a) && (f1 == f1a) && (b4 == b4a) && (f4 == f4a) = Right True
      | otherwise = Right False
    
TopFace b2 f2 b3 f3 === TopFace b2a f2a b3a f3a
      | (b2 == b2a) && (f2 == f2a) && (b3 == b3a) && (f3 == f3a) = Right True
      | otherwise = Right False

RightFace b3 b4 f3 f4 === RightFace b3a b4a f3a f4a
      | (b3 == b3a) && (b4 == b4a) && (f3 == f3a) && (f4 == f4a) = Right True
      | otherwise = Right False

LeftFace b1 b2 f1 f2 === LeftFace b1a b2a f1a f2a
      | (b1 == b1a) && (b2 == b2a) && (f1 == f1a) && (f2 == f2a) = Right True
      | otherwise = Right False

BackFace b1 b2 b3 b4 === BackFace b1a b2a b3a b4a
      | (b1 == b1a) && (b2 == b2a) && (b3 == b3a) && (b4 == b4a)  = Right True
      | otherwise = Right False
    ---------------------------------- cubes --------------------
CubePoints f1 f2 f3 f4 b1 b2 b3 b4 === CubePoints f1a f2a f3a f4a b1a b2a b3a b4a
      | (f1 == f1a) && (f2 == f2a) && (f3 == f3a) && (f4 == f4a) && (b1 == b1a) && ( b2 == b2a) && (b3 == b3a) && (b4 == b4a) = Right True
      | otherwise = Right False

CornerPointsError errMessage' === CornerPointsError errMessage'' =
      Right $ errMessage' == errMessage''

(CornerPointsNothing) === CornerPointsNothing = Right True
(CornerPointsNothing) === _ = Right False
(_) === CornerPointsNothing = Right False

(===) anyThingElse isFalseOrNeedsAPatterMatch = Left $ "CornerPoints.CornerPoints === has missing or illegal pattern match for: " ++
                                                         (TS.showConstructor anyThingElse) ++ " and " ++ (TS.showConstructor isFalseOrNeedsAPatterMatch)


------------------------------------- now on a list-----------------------------------------------------------------
(|===|) :: [CornerPoints] -> [CornerPoints] -> Either String Bool
[] |===| [] = Right True
(x:xs) |===| [] = Right False
[] |===| (y:ys) = Right False
(x:xs) |===| (y:ys) =
  case (x === y) of
    Right False -> Right False
    Left e      -> Left e
    Right True  ->
      (xs) |===| (ys)
--------------------------------------------------- add cubes +++ ---------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
-- || between to lines means done to a list
-- @ is a cornerpoint
-- & is a CornerPointsBuilder 
-- # is a function

{- |Add [CornerPoints] to [CornerPoints].
-}
(|+++|) :: [CornerPoints] -> [CornerPoints] -> [CornerPoints]
c1 |+++| c2 = zipWith (+++) c1 c2

{-- |A lower infix version of +++. Usefull for chaining together +++
Ex: BackBottomLine +++ BottomFrontLine +++$ BackTopLine +++ FrontTopLine
-}
--ToDo: Consider getting rid of it as it is rarely used.
(+++-) :: CornerPoints -> CornerPoints -> CornerPoints
(+++-) = (+++)

{- |A monadic style +++ that adds the result of f input to input.
-}

{- |Similar to +++> except that it can apply any function, rather than just f +++

    So it applies a function to the first argument, and returns another CornerPoint based on that.

    Eg: Transpose a [CornerPoints] upwards in order to create a new layer of [CornerPoints]-}  
(@+++#@) :: CornerPoints -> (CornerPoints -> CornerPoints) -> CornerPoints
(BottomFace b1 f1 b4 f4) @+++#@ f = (BottomFace b1 f1 b4 f4) +++ (f (BottomFace b1 f1 b4 f4))
(CubePoints f1 f2 f3 f4 b1 b2 b3 b4) @+++#@ f = (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) +++ (f (CubePoints f1 f2 f3 f4 b1 b2 b3 b4))
(TopFace b2 f2 b3 f3) @+++#@ f = (TopFace b2 f2 b3 f3) +++ (f (TopFace b2 f2 b3 f3))

-- |@+++#@| ++++>>
-- |Apply @+++#@ to a [CornerPoints].
(|@+++#@|) :: [CornerPoints] -> (CornerPoints -> CornerPoints) -> [CornerPoints]
faces |@+++#@| f = [ x @+++#@ f |  x <- faces]

-- |Building up a shape usually involves [[CornerPoints]]. This allow use of infix operators
--  to build up the shape in an monadic way, along with the use of &+++#@.
data CornerPointsBuilder  = CornerPointsBuilder {getCornerPoints :: [[CornerPoints]]}
  deriving (Eq, Show)
-- |The infix operator to go along with CornerPointsBuilder for building up shapes as [[CornerPoints]]
(&+++#@) :: CornerPointsBuilder -> ([CornerPoints] -> [CornerPoints]) -> CornerPointsBuilder
(CornerPointsBuilder cornerPoints) &+++#@ f = CornerPointsBuilder ( (f $ head cornerPoints) : cornerPoints)

-- |Add each face to the next face, left -> right, resulting in CubePoints.
-- Ex: pass a RightFace into a list of LeftFaces, resulting in a list of CubePoints
(+++>) :: CornerPoints -> [CornerPoints] -> [CornerPoints]
a +++> bs =
     tail $ scanl (+++) a bs


{-Add CornerPoints together.
Must follow all the rules of adding.
Ex: FrontFace can be added to BackFace
but
    FrontFace can't be added to a TopFace.-}
(+++) :: CornerPoints -> CornerPoints -> CornerPoints

(CornerPointsId) +++ anyCornerPoint = anyCornerPoint
anyCornerPoint +++ (CornerPointsId) = anyCornerPoint

(BottomFace b1 f1 b4 f4) +++ (TopFace b2 f2 b3 f3) = 
  CubePoints {f1=f1, f2=f2, f3=f3, f4=f4, b1=b1, b2=b2, b3=b3, b4=b4}

(TopFace b2 f2 b3 f3) +++ (BottomFace b1 f1 b4 f4) = 
  CubePoints {f1=f1, f2=f2, f3=f3, f4=f4, b1=b1, b2=b2, b3=b3, b4=b4}

(BackFace b1 b2 b3 b4) +++ (FrontFace f1 f2 f3 f4) =
    CubePoints {b1=b1, b2=b2, b3=b3, b4=b4, f1=f1, f2=f2, f3=f3, f4=f4}

(FrontFace f1 f2 f3 f4)  +++ (BackFace b1 b2 b3 b4)  =
    CubePoints {b1=b1, b2=b2, b3=b3, b4=b4, f1=f1, f2=f2, f3=f3, f4=f4}

(LeftFace b1 b2 f1 f2) +++ (RightFace b3 b4 f3 f4) =
    CubePoints {b1=b1, b2=b2, b3=b3, b4=b4, f1=f1, f2=f2, f3=f3, f4=f4}

(RightFace b3 b4 f3 f4) +++ (LeftFace b1 b2 f1 f2) =
     CubePoints {b1=b1, b2=b2, b3=b3, b4=b4, f1=f1, f2=f2, f3=f3, f4=f4}


(CubePoints f1 f2 f3 f4 b1 b2 b3 b4) +++ (FrontFace f1r f2r f3r f4r) =
    (BackFace {b1=f1, b2=f2, b3=f3, b4=f4}) +++ (FrontFace f1r f2r f3r f4r) 

(CubePoints f1 f2 f3 f4 b1 b2 b3 b4) +++ (TopFace b2t f2t b3t f3t) =
   (BottomFace {b1=b2, b4=b3, f1=f2, f4=f3}) +++ (TopFace b2t f2t b3t f3t)
---------------------------------------------------------------------------------------------
(CubePoints f1 f2 f3 f4 b1 b2 b3 b4) +++ (BottomFace b1b   f1b b4b f4b) =
   (BottomFace {b1=b1b, b4=b4b, f1=f1b, f4=f4b}) +++ (TopFace b1 f1 b4 f4)

(TopFace b2t f2t b3t f3t) +++ (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) =
   CubePoints {b1=b2, b2=b2t, b3=b3t, b4=b3, f1=f2, f2=f2t, f3=f3t, f4=f3}

(LeftFace b1t b2t f1t f2t) +++ (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) =
  CubePoints {b1=b1t, b4=b1, b2=b2t, b3=b2, f1=f1t, f2=f2t, f3=f2, f4=f1}

(LeftFace b1 b2 f1 f2) +++ (BottomLeftLine b1a f1a) =
  (LeftFace b1a b1 f1a f1)

(CubePoints f1 f2 f3 f4 b1 b2 b3 b4) +++ (LeftFace b1t b2t f1t f2t) =
     CubePoints {b1=b1t, b4=b1, b2=b2t, b3=b2, f1=f1t, f2=f2t, f3=f2, f4=f1}

(FrontFace f1 f2 f3 f4) +++ (BottomFrontLine f1' f4') =
  FrontFace f1' f1 f4 f4'

(RightFace b3t b4t f3t f4t) +++ (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) =
  CubePoints {b1=b4, b4=b4t, b2=b3, b3=b3t, f1=f4, f2=f3, f3=f3t, f4=f4t}

(RightFace b3f b4f f3f f4f) +++ (BottomRightLine b4l f4l) =
  (RightFace b4f b4l f4f f4l)

(CubePoints f1 f2 f3 f4 b1 b2 b3 b4) +++ (RightFace b3t b4t f3t f4t) =
  CubePoints {b1=b4, b4=b4t, b2=b3, b3=b3t, f1=f4, f2=f3, f3=f3t, f4=f4t}

(CubePoints f1 f2 f3 f4 b1 b2 b3 b4) +++ (FrontRightLine f3' f4') =
  FrontFace f4 f3 f3' f4'

(CubePoints f1 f2 f3 f4 b1 b2 b3 b4) +++ (BackRightLine b3' b4') =
  BackFace b4 b3 b3' b4'

(FrontFace f1 f2 f3 f4) +++ (FrontRightLine f3' f4') =
  FrontFace f4 f3 f3' f4'

(FrontFace f1 f2 f3 f4) +++ (FrontLeftLine f1' f2') =
  FrontFace f1' f2' f2 f1

(BackFace b1 b2 b3 b4) +++ (BackRightLine b3' b4') =
  BackFace b4 b3 b3' b4'

(BackFace b1 b2 b3 b4) +++ (BackLeftLine b1' b2') =
   BackFace b1' b2' b2 b1

(BackFace b1t b2t b3t b4t) +++ (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) =
  CubePoints {b1=b1t, b2=b2t, b3=b3t, b4=b4t, f1=b1, f2=b2, f3=b3, f4=b4}

(TopFace b2 f2 b3 f3) +++ (TopRightLine b3t f3t) =
    (TopFace b3 f3 b3t f3t)

(FrontTopLine f2 f3) +++ (BottomFrontLine f1 f4) =
  FrontFace f1 f2 f3 f4

(BackBottomLine b1 b4) +++ (BottomFrontLine f1 f4) =
    BottomFace b1 f1 b4 f4

(BackBottomLine b1 b4) +++ (BackTopLine b2 b3) =
  BackFace b1 b2 b3 b4

(BackTopLine b2 b3) +++ (BackBottomLine b1 b4) =
  BackFace b1 b2 b3 b4

(BottomFrontLine f1 f4) +++ (BackBottomLine b1 b4) =
     BottomFace b1 f1 b4 f4

(BottomRightLine b4 f4) +++ (BottomLeftLine b1 f1) =
    BottomFace b1 f1 b4 f4

(BottomLeftLine b1 f1) +++ (BottomRightLine b4 f4) =
  BottomFace b1 f1 b4 f4

(FrontLeftLine f1 f2) +++ (FrontRightLine f3 f4) =
  FrontFace f1 f2 f3 f4

(FrontRightLine f3 f4) +++ (FrontLeftLine f1 f2)  =
  FrontFace f1 f2 f3 f4

(FrontRightLine f3 f4) +++ (BackRightLine b3 b4)  =
  RightFace b3 b4 f3 f4

(TopLeftLine b2 f2) +++ (TopRightLine b3 f3) =
    TopFace b2 f2 b3 f3

(TopLeftLine b2 f2) +++ (BottomLeftLine b1 f1) =
    LeftFace b1 b2 f1 f2

(TopRightLine b3 f3) +++ (TopLeftLine b2 f2) =
    TopFace b2 f2 b3 f3

(TopRightLine b3 f3) +++ (F3 f3') =
  TopRightLine f3 f3'

(TopRightLine b3 f3) +++ (B3 b3') =
  TopRightLine b3 b3'

(TopRightLine b3 f3) +++ (BottomRightLine b4 f4) =
    (RightFace b3 b4 f3 f4)

(TopLeftLine b2t f2t) +++ (TopFace b2 f2 b3 f3) =
   (TopFace b2t f2t b2 f2)

(TopLeftLine b2 f2) +++ (B2 b2') =
  TopLeftLine b2' b2

(TopFace b2 f2 b3 f3) +++ (TopLeftLine b2t f2t) =
    (TopFace b2t f2t b2 f2)

(BackTopLine b2 b3) +++ (FrontTopLine f2 f3) =
    TopFace b2 f2 b3 f3

(BackTopLine b2 b3) +++ (B2 b2') =
  BackTopLine b2' b2

(BottomFace b1 f1 b4 f4) +++ (BottomFrontLine f1a f4a) =
    BottomFace {b1=f1, b4=f4, f1=f1a, f4=f4a}

(BottomFace b1 f1 b4 f4) +++ (BottomLeftLine b1a f1a) =
    BottomFace {b1=b1a, f1=f1a, b4=b1, f4=f1}

(BottomFace b1 f1 b4 f4) +++ (BottomRightLine b4' f4') =
  BottomFace b4 f4 b4' f4'

(BottomRightLine b4' f4') +++ (BottomFace b1 f1 b4 f4) =
  BottomFace b4 f4 b4' f4'

(TopFace b2 f2 b3 f3) +++ (FrontTopLine f2a f3a) =
    TopFace {b2=f2, b3=f3, f2=f2a, f3=f3a}


(BottomFrontLine f1 f4) +++ (FrontTopLine f2 f3) =
    FrontFace f1 f2 f3 f4

(B1 b1) +++ (B4 b4) =
     BackBottomLine {b1=b1, b4=b4}

(B1 b1) +++ (B2 b2) =
  (BackLeftLine b1 b2)

(B4 b4) +++ (B1 b1) =
    BackBottomLine {b1=b1, b4=b4}

(B2 b2) +++ (B3 b3) =
     BackTopLine {b2=b2, b3=b3}

(B3 b3) +++ (B2 b2) =
  BackTopLine {b2=b2, b3=b3}

(B2 b2) +++ (F2 f2) =
  TopLeftLine b2 f2

(F1 f1) +++ (F4 f4) =
     BottomFrontLine {f1=f1, f4=f4}

(F1 f1) +++ (BottomFrontLine f1a f4a) = BottomFrontLine f1 f1a

--changed this one for flex geox
(BottomFrontLine f1a f4a) +++ (F1 f1) = BottomFrontLine f1 f1a

(F1 f1) +++ (B1 b1) =
    BottomLeftLine b1 f1

(F1 f1) +++ (F2 f2) =
  FrontLeftLine f1 f2

(B1 b1) +++ (F1 f1) =
    BottomLeftLine b1 f1

(B3 b3) +++ (F3 f3) =
  TopRightLine b3 f3

(F2 f2) +++ (F3 f3) =
     FrontTopLine {f2=f2, f3=f3}

(F2 f2) +++ (B2 b2) =
    TopLeftLine b2 f2

(F3 f3) +++ (F2 f2) =
    FrontTopLine {f2=f2, f3=f3}

(F3 f3) +++ (F4 f4) = FrontRightLine f3 f4

(F4 f4) +++ (F3 f3) = FrontRightLine f3 f4

(F3 f3) +++ (B3 b3) =
    (TopRightLine b3 f3)

(F4 f4) +++ (B4 b4) =
    (BottomRightLine b4 f4)

(B4 b4) +++ (F4 f4)  =
    (BottomRightLine b4 f4)

(B4 b4) +++ (B3 b3) = BackRightLine b3 b4

(F4 f4) +++ (F1 f1) =
    (BottomFrontLine f1 f4)


(BottomFrontLine f1 f4t) +++ (F4 f4) =
    BottomFrontLine f4t f4

(FrontTopLine f2 f3) +++ (F3 f3t) =
  FrontTopLine f3 f3t

(FrontTopLine f2 f3) +++ (BackTopLine b2 b3) =
    TopFace b2 f2 b3 f3

(FrontTopLine f2 f3) +++ (F2 f2t) =
  FrontTopLine f2t f2

(F2 f2t) +++ (FrontTopLine f2 f3)  =
  FrontTopLine f2t f2

(BottomRightLine b4' f4') +++ (F4 f4'') =
  BottomRightLine f4' f4''

(BackBottomLine b1 b4) +++ (B1 b1') = BackBottomLine b1' b1
(BackBottomLine b1 b4) +++ (BottomFrontLine f1 f4) = BottomFace b1 f1 f4 f4

(B1 b1') +++ (BackBottomLine b1 b4) = BackBottomLine b1' b1

-- ------------------------ non-standard as used in Delaunay meshes ------------------------------
(RightFace b3 b4 f3 f4) +++ (FrontLeftLine f1 f2) = 
  LeftFace b4 b3 f1 f2
--inverse
(FrontLeftLine f1 f2) +++ (RightFace b3 b4 f3 f4) =
  (RightFace b3 b4 f3 f4) +++ (FrontLeftLine f1 f2)

----------------------------- illegal -----------------------------------------------------------
(CornerPointsError _) +++ b = CornerPointsError "illegal CornerPointsError +++ _ operation"
--faces
(BackFace _ _ _ _) +++ (BackFace _ _ _ _) = CornerPointsError "illegal BackFace +++ BackFace operation"
(BottomFace _ _ _ _) +++ (BottomFace _ _ _ _) = CornerPointsError "illegal BottomFace +++ BottomFace operation"
(FrontFace _ _ _ _) +++ (FrontFace _ _ _ _) = CornerPointsError "illegal FrontFace +++ FrontFace operation"
(LeftFace _ _ _ _) +++ (LeftFace _ _ _ _) = CornerPointsError "illegal LeftFace +++ LeftFace operation"
(RightFace _ _ _ _) +++ (RightFace _ _ _ _) = CornerPointsError "illegal RightFace +++ RightFace operation"
(TopFace _ _ _ _) +++ (TopFace _ _ _ _) = CornerPointsError "illegal TopFace +++ TopFace operation"
--lines
(BackBottomLine _ _) +++ (BackBottomLine _ _) = CornerPointsError "illegal BackBottomLine +++ BackBottomLine operation"
(BackTopLine _ _) +++ (BackTopLine _ _) = CornerPointsError "illegal BackTopLine +++ BackTopLine operation"
(BottomFrontLine _ _) +++ (BottomFrontLine _ _) = CornerPointsError "illegal BottomFrontLine +++ BottomFrontLine operation"
(FrontTopLine _ _) +++ (FrontTopLine _ _) = CornerPointsError "illegal FrontTopLine +++ FrontTopLine operation"

(CornerPointsNothing) +++ _ = CornerPointsNothing
_ +++ (CornerPointsNothing) = CornerPointsNothing

anythingElseIsIllegal +++ orNotPatternMatched = CornerPointsError $ "unmatched or illegal +++ operation of " ++ (TS.showConstructor anythingElseIsIllegal) ++ " " ++ (TS.showConstructor orNotPatternMatched)

{------------------------------------------------------------ ++++ --------------------------------------------------
Add together CornerPoints with using Either.

If handled by +++ simply wrap them in Either.
IF +++ gives CornerPointsError then wrap error msg in Left

This is the way +++ should have been from the start.

-}
-- | +++ used with Either
(++++) :: CornerPoints -> CornerPoints -> Either String CornerPoints

c1 ++++ c2 =
  case c1 +++ c2 of
    CornerPointsError e -> Left e
    goodCornerPoint     -> Right goodCornerPoint
----------------------------------------------- scale cubes/points ------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
{-                                           Over view
Allow the scaling of a shape. This can be done on all 3 axis at once, or on a single axis. So far only the z-axis scale has been created.
 -}


--used to change just the z axis of a CornerPoints
scaleCornerPointsZ :: Double -> CornerPoints -> CornerPoints
scaleCornerPointsZ scaleFactor (CubePoints f1 f2 f3 f4 b1 b2 b3 b4)  = 
    CubePoints 
        {
            f1=scalePointZ f1 scaleFactor,
            f2=scalePointZ f2 scaleFactor,
            f3=scalePointZ f3 scaleFactor,
            f4=scalePointZ f4 scaleFactor,
            b1=scalePointZ b1 scaleFactor,
            b2=scalePointZ b2 scaleFactor,
            b3=scalePointZ b3 scaleFactor,
            b4=scalePointZ b4 scaleFactor
        }

scaleCornerPoints :: Double -> CornerPoints  -> CornerPoints
scaleCornerPoints scaleFactor (CubePoints f1 f2 f3 f4 b1 b2 b3 b4)  = 
    CubePoints 
        {
            f1=scalePoint f1 scaleFactor,
            f2=scalePoint f2 scaleFactor,
            f3=scalePoint f3 scaleFactor,
            f4=scalePoint f4 scaleFactor,
            b1=scalePoint b1 scaleFactor,
            b2=scalePoint b2 scaleFactor,
            b3=scalePoint b3 scaleFactor,
            b4=scalePoint b4 scaleFactor
        }

{------------ scale internal support functions ------------}
scalePoint :: Point -> Double -> Point
scalePoint (Point x y z) scaleFactor = Point {x_axis=x*scaleFactor, y_axis=y*scaleFactor, z_axis=z*scaleFactor}

--used to change just the z axis of a Point
scalePointZ :: Point -> Double -> Point
scalePointZ (Point x y z) scaleFactor = Point {x_axis=x, y_axis=y, z_axis=z*scaleFactor}

{- |
Use Lense to extract a face from [CornerPoints]
-}
getCornerPointsWithIndex :: String -> [CornerPoints] -> Int-> CornerPoints
getCornerPointsWithIndex errMsg cutterFaces index = 
  case (cutterFaces ^? element index) of
    Nothing -> CornerPointsError errMsg 
    Just a  -> a


--this would return a Constr instead, if that would be better.
-- Will need to see how it gets used.
--showMeTheType :: CornerPoints -> Constr
--showMeTheType cpoint = toConstr $ cpoint

-- | Show the type of a CornerPoints.
-- Handy for figuring out missing or illegal pattern matches.
-- Used by +++ to get the types involved for the catchall pattern of:
--unmatched or illegal +++ operation of: <cpoint1> <cpoint2>
cpointType :: CornerPoints -> String
--cpointType cpoint = showConstr . toConstr $ cpoint
cpointType cpoint = TS.showConstructor cpoint

-- | Add together 2 Either CornerPoints and return a Left if a CornerPointsError, else return Right result.
(##+++#) :: Either String CornerPoints -> Either String CornerPoints -> Either String CornerPoints
(Right cPoint1) ##+++# (Right cPoint2) =
  let
    cPointAdded = cPoint1 +++ cPoint2
  in
  case cPointAdded of
    CornerPointsError msg -> Left msg
    otherwise -> Right cPointAdded

(Left e) ##+++# (Right cpoint) =
  Left $ "##+++# error: trailing: " ++ (show cpoint) ++ " and leading Left error: " ++ e

(Right cpoint) ##+++# (Left e) =
  Left $ "##+++# error: leading: " ++ (show cpoint) ++ " and trailing Left error: " ++ e

(Left eLeading) ##+++# (Left eTrailing) =
  Left $ "##+++# error: leading Left error: " ++ eLeading ++ " and trailing Left error: " ++ eTrailing


-- | Add together 2 CornerPoints and return and Left if a CornerPointsError, else return Right result.
(+++#) :: CornerPoints -> CornerPoints -> Either String CornerPoints
cPoint1 +++# cPoint2 =
  let
    cPointAdded = cPoint1 +++ cPoint2
  in
  case cPointAdded of
    CornerPointsError msg -> Left msg
    otherwise -> Right cPointAdded


{-
not tested.
Break a CornerPoints into [Points]
Return an Either list in case of missing pattern match.
Also do the same for a [Cpts]
Known uses:
Break CPts down into points when working with gmsh scripts.
-}
toPoints :: CornerPoints -> Either String [Point]
toPoints (FrontFace f1 f2 f3 f4) = Right [f1,f2,f3,f4]
toPoints (B1 b1) = Right [b1]
toPoints cpt = Left $ "CornerPoints.toPoints: unhandled or illegal patter match for: " ++ (TS.showConstructor cpt )
  
toPointsFromList :: [CornerPoints] -> Either String [Point]
toPointsFromList [] = Right []
toPointsFromList cpts  = toPointsFromList' cpts (Right [])
toPointsFromList' :: [CornerPoints] -> (Either String [Point]) -> Either String [Point]
toPointsFromList' (cpt:[]) (Right pointsSoFar) =
  case (toPoints cpt) of
    Right points -> Right (pointsSoFar ++ points)
    Left e -> Left e
toPointsFromList' (cpt:cpts) (Right pointsSoFar) =
  case (toPoints cpt) of
    Right points -> toPointsFromList' cpts $ Right $ pointsSoFar ++ points
    Left e -> Left e
{-
toPointsFromList :: [CornerPoints] -> Either String [Point]
toPointsFromList [] = Right []
toPointsFromList cpts  = toPointsFromList' cpts (Right [])
toPointsFromList' :: [CornerPoints] -> (Either String [Point]) -> Either String [Point]
toPointsFromList' (cpt:[]) (Right pointsSoFar) =
  case (toPoints cpt) of
    Right points -> Right (points ++ pointsSoFar)
    Left e -> Left e
toPointsFromList' (cpt:cpts) (Right pointsSoFar) =
  case (toPoints cpt) of
    Right points -> toPointsFromList' cpts $ Right $ points ++ pointsSoFar
    Left e -> Left e
-}
