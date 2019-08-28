module Math.Distance(Distance(..), Distant, calculateDistance, getOrdering, fromDistance, calculateXYDistance,
                     DistanceA(..), DistantA, calculateDistanceA, getOrderingA, {-fromDistanceA,-} (<-||->), (<-|->), center, centerA) where

import Math.Equal(equal)

import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.Points(Point(..))
import qualified TypeClasses.Showable as TS
import Helpers.Applicative(extractE)


-- | The disance between 2 Points, or 2 CornerPoints.
-- Known uses: Joiners.Delaunay
data Distance =
  Distance {_distance :: Double}
  |
  NoDistance
 deriving (Show)

fromDistance :: Distance -> Double
fromDistance (Distance d) = d
fromDistance NoDistance = 0.0

instance Eq Distance where
    Distance d == Distance d'
      |  (equal d d')  = True 
      | otherwise = False


-- | Caluculate the Distance between to objects.
-- Currently used by CornerPoints and Points.
class Distant a where
  calculateDistance :: a -> a -> Distance

-- | Compare 2 Distance for an Ordering base on underlying distance.
getOrdering :: Distance ->  Distance -> Ordering
getOrdering    (Distance d) (Distance d')
  | d > d' = GT
  | d < d' = LT
  | d == d' = EQ


----------------------------------------------------------- applicative version-----------------------------
data DistanceA =
  DistanceA {distance :: Double}
 deriving (Show, Ord)
{-
fromDistanceA :: DistanceA -> Double
fromDistanceA (DistanceA d) = d
-}

instance Eq DistanceA where
    DistanceA d == DistanceA d'
      |  (equal d d')  = True 
      | otherwise = False


-- | Caluculate the Distance between to objects.
-- Currently used by CornerPoints and Points.
class DistantA a where
  calculateDistanceA :: a -> a -> Either String DistanceA
  

-- | Compare 2 Distance for an Ordering base on underlying distance.
getOrderingA :: DistanceA ->  DistanceA -> Ordering
getOrderingA    (DistanceA d) (DistanceA d')
  | d > d' = GT
  | d < d' = LT
  | d == d' = EQ


instance Distant CornerPoints where
  calculateDistance  (BottomRightLine f4 b4) (B1 b1) =
     calculateDistance
       (center $ BottomRightLine f4 b4)
       b1
  calculateDistance  (BottomRightLine f4 b4) (F1 f1) =
    calculateDistance
      (center $ BottomRightLine f4 b4)
      f1
  calculateDistance  (BottomLeftLine f1 b1) (B1 b1') =
     calculateDistance
       (center $ BottomLeftLine f1 b1)
       b1'
  calculateDistance  (BottomLeftLine f1 b1) (F1 f1') =
     calculateDistance
       (center $ BottomLeftLine f1 b1)
       f1'
  calculateDistance _ CornerPointsNothing = Distance 0.0
  calculateDistance CornerPointsNothing _ = Distance 0.0



instance DistantA CornerPoints where
  calculateDistanceA (RightFace b3 b4 f3 f4) (FrontLeftLine f1 f2) =
    extractE (calculateDistanceA <$> centerA (RightFace b3 b4 f3 f4) <*> centerA (FrontLeftLine f1 f2))
  calculateDistanceA (FrontLeftLine f1 f2) (RightFace b3 b4 f3 f4) =
    calculateDistanceA (RightFace b3 b4 f3 f4) (FrontLeftLine f1 f2)
  
  calculateDistanceA (RightFace b3 b4 f3 f4) (BackLeftLine b1 b2) =
    extractE (calculateDistanceA <$> centerA (RightFace b3 b4 f3 f4) <*> centerA (BackLeftLine b1 b2))
  calculateDistanceA (BackLeftLine b1 b2) (RightFace b3 b4 f3 f4) =
    calculateDistanceA (RightFace b3 b4 f3 f4) (BackLeftLine b1 b2)

  calculateDistanceA (LeftFace b1 b2 f1 f2) (BackRightLine b3 b4) =
    extractE (calculateDistanceA <$> centerA (LeftFace b1 b2 f1 f2) <*> centerA (BackRightLine b3 b4))
  calculateDistanceA (BackRightLine b3 b4) (LeftFace b1 b2 f1 f2) =
    calculateDistanceA (LeftFace b1 b2 f1 f2) (BackRightLine b3 b4)

  calculateDistanceA (LeftFace b1 b2 f1 f2) (BackLeftLine b1' b2') =
    extractE (calculateDistanceA <$> centerA (LeftFace b1 b2 f1 f2) <*> centerA (BackLeftLine b1' b2'))
  calculateDistanceA (BackLeftLine b1' b2') (LeftFace b1 b2 f1 f2) =
    calculateDistanceA (LeftFace b1 b2 f1 f2) (BackLeftLine b1' b2')


  calculateDistanceA (LeftFace b1 b2 f1 f2) (FrontLeftLine f1' f2') =
    extractE (calculateDistanceA <$> centerA (LeftFace b1 b2 f1 f2) <*> centerA (FrontLeftLine f1' f2'))
  calculateDistanceA (FrontLeftLine f1' f2') (LeftFace b1 b2 f1 f2) =
    calculateDistanceA (LeftFace b1 b2 f1 f2) (FrontLeftLine f1' f2')
  
  calculateDistanceA  (BottomRightLine b4 f4) (B1 b1) =
    extractE (calculateDistanceA <$> centerA (BottomRightLine b4 f4) <*> centerA (B1 b1))
  calculateDistanceA (B1 b1)  (BottomRightLine b4 f4)  =
    calculateDistanceA  (BottomRightLine b4 f4) (B1 b1)
  
  calculateDistanceA  (BottomRightLine b4 f4) (F1 f1) =
    extractE (calculateDistanceA <$> centerA (BottomRightLine b4 f4) <*> centerA (F1 f1))
  calculateDistanceA (F1 f1)  (BottomRightLine b4 f4)  =
    calculateDistanceA  (BottomRightLine b4 f4) (F1 f1)

  calculateDistanceA  (BottomRightLine b4 f4) (BottomLeftLine b1 f1) =
    extractE (calculateDistanceA <$> centerA (BottomRightLine b4 f4) <*> centerA (BottomLeftLine b1 f1))
  calculateDistanceA (BottomLeftLine b1 f1) (BottomRightLine b4 f4) =
    calculateDistanceA  (BottomRightLine b4 f4) (BottomLeftLine b1 f1)

  calculateDistanceA  (BottomLeftLine b1 f1) (BottomLeftLine b1' f1') =
    extractE (calculateDistanceA <$> centerA (BottomLeftLine b1 f1) <*> centerA (BottomLeftLine b1' f1'))
  
  calculateDistanceA  (BottomRightLine b4 f4) (B4 b4') =
    extractE (calculateDistanceA <$> centerA (BottomRightLine b4 f4) <*> centerA (B4 b4'))
  calculateDistanceA (B4 b4')  (BottomRightLine b4 f4)  =
    calculateDistanceA  (BottomRightLine b4 f4) (B4 b4')
  
  calculateDistanceA  (BottomLeftLine b1 f1) (B1 b1') =
    extractE (calculateDistanceA <$> centerA (BottomLeftLine b1 f1) <*> centerA (B1 b1'))
  calculateDistanceA   (B1 b1') (BottomLeftLine b1 f1) =
    calculateDistanceA  (BottomLeftLine b1 f1) (B1 b1')

  calculateDistanceA (TopRightLine b3 f3) (B2 b2) =
    extractE (calculateDistanceA <$> centerA (TopRightLine b3 f3) <*> centerA (B2 b2))
  calculateDistanceA (B2 b2) (TopRightLine b3 f3) =
    calculateDistanceA (TopRightLine b3 f3) (B2 b2) 
    
  calculateDistanceA  (B1 b1) (F1 f1) =
    calculateDistanceA (b1) (f1)
  calculateDistanceA   (F1 f1) (B1 b1)  =
    calculateDistanceA  (B1 b1) (F1 f1)

  calculateDistanceA  (B4 b4) (B4 b4') =
    calculateDistanceA (b4) (b4')

  calculateDistanceA  (B4 b4) (F4 f4) =
    calculateDistanceA (b4) (f4)
  calculateDistanceA   (F4 f4) (B4 b4) =
    calculateDistanceA  (B4 b4) (F4 f4)

  calculateDistanceA  (B4 b4) (F1 f1) =
    calculateDistanceA (b4) (f1)
  calculateDistanceA   (F1 f1) (B4 b4) =
    calculateDistanceA  (B4 b4) (F1 f1)

  calculateDistanceA  (BottomLeftLine b1 f1) (B4 b4) =
    extractE (calculateDistanceA <$> centerA (BottomLeftLine b1 f1) <*> centerA (B4 b4))
  calculateDistanceA   (B4 b4) (BottomLeftLine b1 f1) =
    calculateDistanceA  (BottomLeftLine b1 f1) (B4 b4)

  calculateDistanceA  (BottomLeftLine b1 f1) (F1 f1') =
    extractE (calculateDistanceA <$> centerA (BottomLeftLine b1 f1) <*> centerA (F1 f1'))
  calculateDistanceA   (F1 f1') (BottomLeftLine b1 f1) =
    calculateDistanceA  (BottomLeftLine b1 f1) (F1 f1')

  calculateDistanceA (TopRightLine b3 f3) (F2 f2) =
    extractE (calculateDistanceA <$> centerA (TopRightLine b3 f3) <*> centerA (F2 f2))
  calculateDistanceA (F2 f2) (TopRightLine b3 f3)  =
    calculateDistanceA (TopRightLine b3 f3) (F2 f2)

  calculateDistanceA (TopLeftLine b2 f2) (B2 b2') =
    extractE (calculateDistanceA <$> centerA (TopLeftLine b2 f2) <*> centerA (B2 b2'))
  calculateDistanceA (B2 b2') (TopLeftLine b2 f2)  =
    calculateDistanceA (TopLeftLine b2 f2) (B2 b2')

  -----------------------------------------------------------------
  calculateDistanceA (TopLeftLine b2 f2) (F2 f2') =
    extractE (calculateDistanceA <$> centerA (TopLeftLine b2 f2) <*> centerA (F2 f2'))
  calculateDistanceA (F2 f2') (TopLeftLine b2 f2)  =
    calculateDistanceA (TopLeftLine b2 f2) (F2 f2')
  
  calculateDistanceA (RightFace b3 b4 f3 f4) (BackRightLine b3' b4') =
    extractE (calculateDistanceA <$> centerA (RightFace b3 b4 f3 f4) <*> centerA (BackRightLine b3' b4'))
  calculateDistanceA (BackRightLine b3' b4') (RightFace b3 b4 f3 f4) =
    calculateDistanceA (RightFace b3 b4 f3 f4) (BackRightLine b3' b4')

  calculateDistanceA (BackRightLine b3 b4) (FrontRightLine  f3 f4) =
    extractE (calculateDistanceA <$> centerA (BackRightLine b3 b4) <*> centerA  (FrontRightLine  f3 f4))
  calculateDistanceA (FrontRightLine  f3 f4) (BackRightLine b3 b4) =
    calculateDistanceA (BackRightLine b3 b4) (FrontRightLine  f3 f4)
  
  calculateDistanceA  cpoint1 cpoint2 =
    Left $ "CornerPoints.calculateDistanceA: missing pattern match for: " ++ (TS.showConstructor cpoint1) ++  " and "  ++ (TS.showConstructor cpoint2)

  
   
-- | Given a 2 points, caluclate the distance between the points.
instance Distant Point where
  calculateDistance    point1   point2  =
    let
        distance :: Point -> Point -> Point
        distance    (Point x y z)    (Point x1 y1 z1) =
          --Point (abs $ x - x1) (abs $ y - y1) (abs $ z - z1)
          Point (x - x1) (y - y1) (z - z1)
        p = distance point1 point2
        x = x_axis p
        y = y_axis p
        z = z_axis p
    in 
        Distance $ sqrt (x**2 + y**2 + z**2)

-- | Given a 2 points, caluclate the distance between the points.
{-
instance DistantA Point where
  calculateDistanceA    point1   point2  =
    let
        distance :: Point -> Point -> Point
        distance    (Point x y z)    (Point x1 y1 z1) =
          --Point (abs $ x - x1) (abs $ y - y1) (abs $ z - z1)
          Point (x - x1) (y - y1) (z - z1)
        p = distance point1 point2
        x = x_axis p
        y = y_axis p
        z = z_axis p
    in 
        Right $ Just $ DistanceA $ sqrt (x**2 + y**2 + z**2)
-}
instance DistantA Point where
  calculateDistanceA    point1   point2  =
    let
        distance :: Point -> Point -> Point
        distance    (Point x y z)    (Point x1 y1 z1) =
          --Point (abs $ x - x1) (abs $ y - y1) (abs $ z - z1)
          Point (x - x1) (y - y1) (z - z1)
        p = distance point1 point2
        x = x_axis p
        y = y_axis p
        z = z_axis p
    in 
        Right $ DistanceA $ sqrt (x**2 + y**2 + z**2)

--ToDo: this is to be a fx of Distant
calculateXYDistance :: Point -> Point -> Distance
calculateXYDistance    point1   point2  =
  let
      distance :: Point -> Point -> Point
      distance    (Point x y z)    (Point x1 y1 z1) =
        Point (x - x1) (y - y1) (z - z1)
      p = distance point1 point2
      x = x_axis p
      y = y_axis p
      z = z_axis p
  in
      Distance $ sqrt (x**2 + y**2)

-- | Of a Point(s) or CornerPoint(s):
-- Find the center. For a line such as FrontLeftLine, this would be the middle of the line.
class Center a where
  (<-|->) :: a -> a -> Point -- ^ Takes 2 <CornerPoints/Points> and returns the center Point between them
  center :: a -> Point -- ^ Return the center of the <CornerPoint/Point>. 

instance Center Point where
  center p = p
  (Point x1 y1 z1) <-|-> (Point x2 y2 z2) =
    let
      x = (x1 + x2)/2
      y = (y1 + y2)/2
      z = (z1 + z2)/2
    in
      Point x y z
------------------------------------- center Either----------------------------------------------
{-
Is there a point in making this Either.
Any applicative work should be done above this level.
-}

class CenterA a where
  --(<-||->) :: Either String a -> Either String a -> Either String Point -- ^ Takes 2 <CornerPoints/Points> and returns the center Point between them
  (<-||->) :: a -> a -> Either String Point -- ^ Takes 2 <CornerPoints/Points> and returns the center Point between them
  centerA :: a -> Either String Point -- ^ Return the center of the <CornerPoint/Point>. 

instance CenterA Point where
  centerA (Point x y z) = Right $ Point x y z
  
  --(Right (Point x1 y1 z1)) <-||-> (Right (Point x2 y2 z2)) =
  ((Point x1 y1 z1)) <-||-> ((Point x2 y2 z2)) =
    let
      x = (x1 + x2)/2
      y = (y1 + y2)/2
      z = (z1 + z2)/2
    in
      Right $ Point x y z

  --centerA (Left e) _ = Left e
  --centerA _ (Left e) = Left e
  
instance CenterA CornerPoints where
  centerA (B1 point) = Right point
  centerA (B2 point) = Right point
  centerA (B4 point) = Right point
  centerA (F1 point) = Right point
  centerA (F2 point) = Right point
  centerA (F4 point) = Right point
  centerA (BottomLeftLine b1 f1) = b1 <-||-> f1
  centerA (BottomRightLine f4 b4) = f4 <-||-> b4
  centerA (BackLeftLine b1 b2) = b1 <-||-> b2
  centerA (FrontLeftLine f1 f2) = f1 <-||-> f2
  centerA (BackRightLine b3 b4) = b3 <-||-> b4
  centerA (FrontRightLine f3 f4) = f3 <-||-> f4
  centerA (TopRightLine b3 f3) = b3 <-||-> f3
  centerA (TopLeftLine b2 f2) = b2 <-||-> f2
  centerA (LeftFace b1 b2 f1 f2) =
    case (b1 <-||-> b2) of
      Left e -> Left e
      Right p ->
        case (f1<-||-> f2) of
          Left e -> Left e
          Right p' ->
            p <-||-> p'
  centerA (RightFace b3 b4 f3 f4) =
    case (b3 <-||-> b4) of
      Left e -> Left e
      Right p ->
        case (f3 <-||-> f4) of
          Left e -> Left e
          Right p' ->
            p <-||-> p'
  centerA cpoint =
    Left $ "Math.Distance.centerA(Cornerpoints) has missing pattern match for: " ++ (TS.showConstructor cpoint)
  
{-
  centerA (Right (RightFace b3 b4 f3 f4)) = ((Right b3) <-||-> (Right b4)) <-||-> ((Right f3) <-||-> (Right f4))
  centerA (Right (BackLeftLine b1 b2)) = (Right b1) <-||-> (Right b2)
  centerA (Right (FrontLeftLine f1 f2)) = (Right f1) <-||-> (Right f2)

  
  (Right (BottomLeftLine b1 f1)) <-||-> (Right (B1 b1')) =
    (Right (b1')) <-||-> (centerA $ Right (BottomLeftLine b1 f1))
  (Right (B1 b1')) <-||-> (Right (BottomLeftLine b1 f1)) =
    (Right (BottomLeftLine b1 f1)) <-||-> (Right (B1 b1'))

  (Right (BottomRightLine f4 b4)) <-||-> (Right (B1 b1)) =
    (centerA $ Right (BottomRightLine f4 b4)) <-||-> (Right (b1))
-}

-- ToDo: Fill in missing pattern matches.
instance Center CornerPoints where
  center (B1 point) = point
  center (B4 point) = point
  center (F1 point) = point
  center (F4 point) = point
  center (BottomLeftLine b1 f1) = b1 <-|-> f1
  center (BottomRightLine f4 b4) = f4 <-|-> b4
  center (LeftFace b1 b2 f1 f2) = (b1 <-|-> b2) <-|-> (f1 <-|-> f2)
  center (RightFace b3 b4 f3 f4) = (b3 <-|-> b4) <-|-> (f3 <-|-> f4)
  center (BackLeftLine b1 b2) = b1 <-|-> b2
  center (FrontLeftLine f1 f2) = f1 <-|-> f2

