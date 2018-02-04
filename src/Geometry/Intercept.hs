{- |
Do 2 lines intercept.
Either String Maybe Bool

If so, do they intercept on an endpoint (cornerPoint)
Either String Maybe Bool

If so, what is the point of interception
-Either String Maybe point, or cornerpoint?
 -A Face could intercept along a line, which would be CornerPoints
 -2 lines would intercept at a Pointx
 -maybe need separate modules for Point vs CornerPoints
-}
module Geometry.Intercept(getChangeInX, getChangeInY, yIntercept, lineIntersection, onTheLine,
                          legalIntersection, perimetersContainIllegalIntersection,
                          perimetersContainLegalIntersections, segmentIntersection,
                          closestPointOnLineParamGloss) where

import CornerPoints.CornerPoints(CornerPoints(..), cpointType, (+++))
import CornerPoints.Points (Point(..))

import Math.Distance(DistanceA(..), calculateDistanceA, calculateXYDistance, getOrderingA, center, (<-|->), fromDistance)

import Helpers.Applicative(extractE, extractMaybe)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader

--lineIntersection = lineIntersectionGloss

--legalIntersection = legalIntersectionGloss -------------------------------------------------------------------------------------------------------------------------------
--legalIntersection = legalIntersectionBool
-------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
------------------------------------------------gloss---------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

-- https://hackage.haskell.org/package/gloss-1.11.1.1/docs/src/Graphics-Gloss-Geometry-Line.html#intersectLineLine
--from the Gloss package.
--I added the z axis

lineIntersection :: CornerPoints -> CornerPoints -> Either String (Maybe Point)
lineIntersection (BottomLeftLine (Point ax ay az) (Point bx by bz) ) (BackBottomLine (Point px py pz) (Point qx qy qz)) =
  Right $ lineIntersection' (Point ax ay az) (Point bx by bz) (Point px py pz) (Point qx qy qz)

lineIntersection (FrontTopLine (Point ax ay az) (Point bx by bz) ) (BackTopLine (Point px py pz) (Point qx qy qz)) =
  Right $ lineIntersection' (Point ax ay az) (Point bx by bz) (Point px py pz) (Point qx qy qz)

lineIntersection (TopLeftLine (Point ax ay az) (Point bx by bz) ) (BackTopLine (Point px py pz) (Point qx qy qz)) =
  Right $ lineIntersection' (Point ax ay az) (Point bx by bz) (Point px py pz) (Point qx qy qz)
lineIntersection (BackTopLine (Point px py pz) (Point qx qy qz)) (TopLeftLine (Point ax ay az) (Point bx by bz) )  =
  Right $ lineIntersection' (Point ax ay az) (Point bx by bz) (Point px py pz) (Point qx qy qz)

lineIntersection (BottomRightLine b4 f4) (BackBottomLine b1 b4') =
  Right $ lineIntersection' b4 f4 b1 b4'

lineIntersection c1 c2 =
  Left $ "Geometry.Intercept.lineIntersection missing pattern match for " ++ (cpointType c1) ++ " and " ++ (cpointType c2)

lineIntersection'
        :: Point        -- ^ `P1`
        -> Point        -- ^ `P2`
        -> Point        -- ^ `P3`
        -> Point        -- ^ `P4`
        -> Maybe Point

lineIntersection' (Point x1 y1 z1) (Point x2 y2 z2) (Point x3 y3 z3) (Point x4 y4 z4)
 = let  dx12    = x1 - x2
        dx34    = x3 - x4

        dy12    = y1 - y2
        dy34    = y3 - y4

        --dz12    = z1 - z2
        --dz34    = z3 - z4
        
        den     = dx12 * dy34  - dy12 * dx34
        

   in if den == 0
        then Nothing
        else let
                det12   = x1*y2 - y1*x2
                det34   = x3*y4 - y3*x4 

                numx    = det12 * dx34 - dx12 * det34
                numy    = det12 * dy34 - dy12 * det34
             in Just $ Point (numx / den) ( numy / den) z1 --what to return for the z as I am only looking at intersection in 2D

--comes from gloss intersectSegSeg
segmentIntersection :: CornerPoints -> CornerPoints -> Either String (Maybe Point)
segmentIntersection (BottomLeftLine b1 f1 ) (BackBottomLine b1' b4) =
  segmentIntersection' b1 f1 b1' b4
  
segmentIntersection (BottomRightLine b4 f4) (BackBottomLine b1' b4') =
  segmentIntersection' b4 f4 b1' b4'
  
segmentIntersection (BackTopLine p1a p1b)(TopLeftLine p2a p2b) =
  segmentIntersection'  p1a p1b p2a p2b
segmentIntersection (TopLeftLine p2a p2b) (BackTopLine p1a p1b) =
  segmentIntersection (BackTopLine p1a p1b) (TopLeftLine p2a p2b) 
  
segmentIntersection c1 c2 =
  Left $ "segmentIntersection: missing pattern match for: " ++ (cpointType c1) ++ " and " ++ (cpointType c2)


--rewrote it but still give wrong answer
--Replace with new within Radius distance idea
segmentIntersectionGloss' :: Point -> Point -> Point -> Point -> Either String (Maybe Point)
segmentIntersectionGloss' p1 p2 p3 p4 =
      let
        pIntersection = lineIntersection' p1 p2 p3 p4
      in
      case pIntersection of
        Nothing -> Right Nothing
        Just pIntersect ->
          let 
            t12 = closestPointOnLineParamGloss p1 p2 pIntersect
            t23 = closestPointOnLineParamGloss p3 p4 pIntersect
          in
          case (t12 >= 0 && t12 <= 1) && (t23 >= 0 && t23 <= 1) of
            True -> Right pIntersection
            False -> Right Nothing

{-keep this orig, as it give same incorrect result.
--see: https://wiki.haskell.org/Pattern_guard on how this gaurd patten works
segmentIntersection' :: Point -> Point -> Point -> Point -> Either String (Maybe Point)
segmentIntersection' p1 p2 p3 p4
        | (Just pIntersection)     <- lineIntersection' p1 p2 p3 p4
        , t12           <- closestPointOnLineParamGloss p1 p2 pIntersection
        , t23           <- closestPointOnLineParamGloss p3 p4 pIntersection
        , t12 >= 0 && t12 <= 1
        , t23 >= 0 && t23 <= 1
        = Right $ Just pIntersection
        
        | otherwise
        = Right $ Nothing

-}

{-

-}
segmentIntersection' :: Point -> Point -> Point -> Point -> Either String (Maybe Point)
segmentIntersection' p1 p2 p3 p4 =
  let
        pointOfIntersection = lineIntersection' p1 p2 p3 p4
      in
      case pointOfIntersection of
        Nothing -> Right Nothing
        Just pIntersect ->
          let
          --get the center between p1 p2 which will be used for the radius
          p1p2Center =  p1 <-|-> p2
          --get radius of p1 p2
          p1p2Radius = calculateXYDistance p1p2Center p1
          --is distance from center of p1 p2 to pIntersect <= length of radius of of p1 p2
          p1p2RadiusToPIntersectDist = calculateXYDistance p1p2Center pIntersect
            --tell me the pIntersect is on the p1 p2 segment
          pIntersectIsOnp1p2Segment =  (fromDistance p1p2RadiusToPIntersectDist) <= (fromDistance p1p2Radius)

          --get the center between p1 p2 which will be used for the radius
          p3p4Center = p3 <-|-> p4
          --get radius of p1 p4
          p3p4Radius = calculateXYDistance p3p4Center p3
          --is distance from center of p3 p4 to pIntersect <= length of radius of of p3 p4
          p3p4RadiusToPIntersectDist = calculateXYDistance p3p4Center pIntersect
            --tell me the pIntersect is on the p3 p4 segment
          pIntersectIsOnp3p4Segment =  (fromDistance p3p4RadiusToPIntersectDist) <= (fromDistance p3p4Radius)
          
          in
          --if both segments contain pIntersect return it
          case pIntersectIsOnp1p2Segment && pIntersectIsOnp3p4Segment of
            True -> Right pointOfIntersection
            False -> Right Nothing
          --otherwise nothing

legalIntersection :: CornerPoints -> --advancingCpoint may intersect 
                     CornerPoints -> --a perimeter cpoint
                     Either String (Bool) --legally: no intersection, or intersection at a vertice. Error: thrown by legalIntersectionGenericForLines or missing pattern match.
legalIntersection CornerPointsNothing _ = Right True
legalIntersection _ CornerPointsNothing = Right True
legalIntersection (BottomLeftLine b1 f1 ) (BackBottomLine b1' b4) =
  legalIntersectionGenericForLines b1 f1 b1' b4

--this should be where german hiker is failing
legalIntersection (TopLeftLine b2 f2) (BackTopLine b2' b3') =
  legalIntersectionGenericForLines b2 f2 b2' b3'

legalIntersection (TopRightLine p1 p2) (BackTopLine p3 p4) =
  legalIntersectionGenericForLines p1 p2 p3 p4

legalIntersection (BottomRightLine b4 f4) (BackBottomLine b1' b4') =
  legalIntersectionGenericForLines b4 f4 b1' b4'

legalIntersection (BackTopLine p1 p2) (TopLeftLine p3 p4) =
  legalIntersectionGenericForLines p1 p2 p3 p4
  

legalIntersection advancingCpoint perimeter =
  Left $ "Geometry.Intercept.legalIntersection has missing or illegal pattern match for advancingCpoint: " ++ (cpointType advancingCpoint) ++ " and  perimeter: " ++ (cpointType perimeter)

-- create a generic fx for lines for legalIntersection.
legalIntersectionGenericForLines ::
                     Point -> Point -> Point -> Point -> 
                     Either String (Bool)
legalIntersectionGenericForLines (Point xa ya za) (Point xb yb zb) (Point xa' ya' za') (Point xb' yb' zb') =
  let
    
    
    hasNoIntersectionOrIsOnVertice :: Point -> Point -> -- perimeter as generic points
                        Maybe Point -> --point of intersection, which can now come from segmentIntersection
                        Either String Bool
    hasNoIntersectionOrIsOnVertice  _ _ Nothing = Right True
    hasNoIntersectionOrIsOnVertice  (Point xa' ya' za') (Point xb' yb' zb') pointOfIntersection  =
        case pointOfIntersection of
          Nothing -> Right True
          Just pointOfIntersection ->
            Right $ (pointOfIntersection ==  (Point xa' ya' za')) || (pointOfIntersection ==  (Point xb' yb' zb')) 
      
  in 
    extractE $
     hasNoIntersectionOrIsOnVertice (Point xa' ya' za') (Point xb' yb' zb') <$>
                                              segmentIntersection' (Point xa ya za) (Point xb yb zb) (Point xa' ya' za') (Point xb' yb' zb') 


closestPointOnLineParamGloss
        :: Point        -- ^ `P1`
        -> Point        -- ^ `P2`
        -> Point        -- ^ `P3`
        -> Double
--unlike gloss, I cx'd for /0 when lines are perpendicular, in which case I return numerator
--what should be returned for z
--perhaps need to look at cross product instead of dot product. See safaribooks: 3d math primer for graphics, 2,12
closestPointOnLineParamGloss (Point x1 y1 z1) (Point x2 y2 z2) (Point ix iy iz) =
  let
    numerator = ((Point (ix - x1)(iy - y1){-(iz - z1)-}0) `dotV` (Point (x2 - x1)(y2 - y1){-(z2 - z1)-}0))
    denominator = ((Point (x2 - x1)(y2 - y1){-(z2 - z1)-}0) `dotV` (Point (x2 - x1)(y2 - y1){-(z2 - z1)-}0))
  in
  -- jhw: added in all the 0 checks. Still gives wrong answer. Could go back to cx'g for 0 denominator.
  case denominator == 0 of
    True -> 0 --numerator
    False ->
      case numerator == 0 of
        True -> 0
        False ->
          numerator / denominator

--comes from gloss
-- | The dot product of two vectors.
dotV :: Point -> Point -> Double
dotV (Point x1  y1 z1) (Point x2  y2 z2) =
  
  x1 * y1 + x2 * y2 


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
------------------------------------------------end of gloss--------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------




--fails for illegal intersection in 2nd pos of 2nd list in perimeters
--test segmentInterceptTest3 shows the intercept
--need to test to see if legal
perimetersContainIllegalIntersection :: [[CornerPoints]] -> CornerPoints -> Either String Bool
perimetersContainIllegalIntersection (p:perimeters) cpoint =
  let
    perimetersContainIllegalIntersection' :: [CornerPoints] -> CornerPoints -> Either String Bool
    perimetersContainIllegalIntersection' (p:ps) cpoint =
      case legalIntersection cpoint p of
        --legalInnerPerimTest3 test when points run in this order
      --case legalIntersection p cpoint of
        --legalInnerPerimTest3 test fails if reverse the points
        Left e -> Left $ "perimetersContainIllegalIntersection error: " ++  e
        Right True -> perimetersContainIllegalIntersection' ps cpoint
        Right False -> Right True
    perimetersContainIllegalIntersection' ([]) _ = Right False

  in
    case perimetersContainIllegalIntersection' p cpoint of
      Left e -> Left e
      Right False ->
        perimetersContainIllegalIntersection perimeters cpoint
      Right True -> Right True

perimetersContainIllegalIntersection [] _ = Right False

perimetersContainLegalIntersections :: [[CornerPoints]] -> CornerPoints -> Either String Bool
perimetersContainLegalIntersections perimeters cpoint = do
  containIllegal <- perimetersContainIllegalIntersection perimeters cpoint
  return $ not containIllegal
  

-- http://bit-player.org/wp-content/extras/bph-publications/BeautifulCode-2007-Hayes.pdf
--is this not intersection of lines again, without knowing about segements
  --Not even that, as it takes three point, and sees if they are on the same line. But::::
--order does not matter, is checking if they are all in a line, in any order
--return 0 if true(all in a line) else some other value
onTheLine :: Point -> Point -> Point -> Double
onTheLine (Point x1 y1 z1) (Point x2 y2 z2) (Point x3 y3 z3) =
  (x1 - x3) * (y2 - y3) - (x2 - x3) * (y1 - y3)


{-
calculate the slope of the line
-get range of x
-get range of y
 -polarity will matter, is y <in/de>creasing as x increases or stays the same
 -so always calculate in same way, front to back, btm to top, or left to right
  -some CornerPoints, such as a <Left/Right...>Face could be measured in more than 1 of these ways, so do it in the given order

-}
getChangeInX :: CornerPoints -> Either String DistanceA
getChangeInX (BottomLeftLine b1 f1) =
  Right $ DistanceA $ (x_axis f1) - (x_axis b1)

getChangeInX (BackBottomLine b1 b4) =
  Right $ DistanceA $ (x_axis b1) - (x_axis b4)

getChangeInX cpoint =
  Left $ "Geometry.Intercept.getChangeInX: illegal or missing pattern match for " ++ (cpointType cpoint)

getChangeInY :: CornerPoints -> Either String DistanceA
getChangeInY (BottomLeftLine b1 f1) =
  Right $ DistanceA $ (y_axis f1) - (y_axis b1)

getChangeInY (BackBottomLine b1 b4) =
  Right $ DistanceA $ (y_axis b1) - (y_axis b4)

getChangeInY cpoint =
  Left $ "Geometry.Intercept.getChangeInY: illegal or missing pattern match for " ++ (cpointType cpoint)

divDistanceA :: DistanceA -> DistanceA -> DistanceA 
divDistanceA (DistanceA d1) (DistanceA d2) =  DistanceA $ d1 / d2

multDistanceA :: DistanceA -> DistanceA -> DistanceA 
multDistanceA (DistanceA d1) (DistanceA d2) =  DistanceA $ d1 * d2

addDistanceA :: DistanceA -> DistanceA -> DistanceA 
addDistanceA (DistanceA d1) (DistanceA d2) =  DistanceA $ d1 + d2

subtractDistanceA :: DistanceA -> DistanceA -> DistanceA 
subtractDistanceA (DistanceA d1) (DistanceA d2) =  DistanceA $ d1 - d2

setX :: Point -> DistanceA -> Point
setX (Point x y z) (DistanceA d) = Point (x + d) y z
--geomalgorithms.com


--my algorithm which is nfg
yIntercept :: CornerPoints -> CornerPoints -> Either String Point
yIntercept (BottomLeftLine b1 f1) (BackBottomLine b1' b4') =
  let bllchangeInY = getChangeInY  (BottomLeftLine b1 f1)
      bllchangeInX = getChangeInX  (BottomLeftLine b1 f1)
  
      bblchangeInY = getChangeInY  (BackBottomLine b1' b4')
      bblchangeInX = getChangeInX  (BackBottomLine b1' b4')
  
      rateOfChange = divDistanceA <$>  bllchangeInY <*> bblchangeInY
      diffInY = (y_axis f1) - (y_axis b1')  
      -- x_f1 = 
   
      {-
      (rate of change * diff in y of bll and bbl ) + x_axis f1
       ================temp======================
      -}
      temp = (multDistanceA) <$> (Right (DistanceA diffInY)) <*> rateOfChange
      temp2 = (addDistanceA) <$> temp <*> (Right (DistanceA (x_axis f1)))
      temp3 = setX <$> Right (f1) <*> temp2 
      
  in temp3
      --Left "filler to compile"

test (BottomLeftLine b1 f1) (BackBottomLine b1' b4') =
  let bllchangeInY = getChangeInY  (BottomLeftLine b1 f1)
      bllchangeInX = getChangeInX  (BottomLeftLine b1 f1)

      bblchangeInY = getChangeInY  (BackBottomLine b1' b4')
      bblchangeInX = getChangeInX  (BackBottomLine b1' b4')
  in  divDistanceA <$>  bllchangeInY <*> bblchangeInY
