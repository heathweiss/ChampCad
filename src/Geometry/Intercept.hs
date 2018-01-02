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
module Geometry.Intercept(getChangeInX, getChangeInY, yIntercept, topCoderAreTheParallel, topCoderXRayIntercept, lineIntersection, onTheLine,
                          segmentIntersection, segmentIntersectionBreakDown, legalIntersection, perimetersContainIllegalIntersection) where

import CornerPoints.CornerPoints(CornerPoints(..), cpointType)
import CornerPoints.Points (Point(..))

import Math.Distance(DistanceA(..), calculateDistanceA, getOrderingA)

import Helpers.Applicative(extractE, extractMaybe)

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


--topCoder.com solution
--NFG: Maybe just don't understand what they are saying.
topCoderAreTheParallel :: CornerPoints -> CornerPoints -> Either String Bool
topCoderAreTheParallel (BottomLeftLine b1 f1) (BackBottomLine b1' b4') =
  let
    
    a1 = getChangeInY  (BottomLeftLine b1 f1)
    a2 = getChangeInY  (BackBottomLine b1' b4')
    b1Cx = getChangeInX  (BottomLeftLine b1 f1)
    b2Cx = getChangeInX (BackBottomLine b1' b4')

    a1xb2Cx = multDistanceA <$> a1 <*> b2Cx
    a2xb1Cx = multDistanceA <$> a2 <*> b1Cx
  in
    (==) <$> a1xb2Cx <*> a2xb1Cx 
      

--cx for intercept of 2 Rays (inifinite lines)
topCoderXRayIntercept :: CornerPoints -> CornerPoints -> Either String (Maybe CornerPoints)
topCoderXRayIntercept (BottomLeftLine b1 f1) (BackBottomLine b1' b4') =
  let
    a1 = getChangeInY  (BottomLeftLine b1 f1)
    a2 = getChangeInY  (BackBottomLine b1' b4')
    b1Cx = getChangeInX  (BottomLeftLine b1 f1)
    b2Cx = getChangeInX (BackBottomLine b1' b4')
    
    a1xb2Cx = multDistanceA <$> a1 <*> b2Cx
    a2xb1Cx = multDistanceA <$> a2 <*> b1Cx

    --C = A*x1+B*y1
    c1' = multDistanceA <$> a1 <*> Right (DistanceA $ x_axis b1)  
    c1'' = multDistanceA <$>  b1Cx <*> Right (DistanceA $ y_axis b1)
    c1 = addDistanceA <$> c1' <*> c1''

    c2' = multDistanceA <$> a2 <*> Right (DistanceA $ x_axis b1')  
    c2'' = multDistanceA <$>  b2Cx <*> Right (DistanceA $ y_axis b1')
    c2 = addDistanceA <$> c2' <*> c2''

    det =  subtractDistanceA <$> a1xb2Cx <*> a2xb1Cx

    {-
    temp' =  multDistanceA <$> b2Cx <*> c1
    temp'' = multDistanceA <$> b1Cx <*> c2
    temp  = addDistanceA <$> temp' <*> temp''-}

    --xIntercept = divDistanceA <$> temp <*> a1xb2Cx
    xIntercept = divDistanceA <$> (subtractDistanceA <$> (multDistanceA <$> b2Cx <*> c1)  <*> (multDistanceA <$> b1Cx <*> c2)) <*> a1xb2Cx
    yIntercept = divDistanceA <$> (subtractDistanceA <$> (multDistanceA <$> a1 <*> c2)  <*> (multDistanceA <$> a2 <*> c1)) <*> a1xb2Cx

    cpointSet :: (Point -> CornerPoints) -> DistanceA -> DistanceA  -> DistanceA -> CornerPoints
    cpointSet constructor (DistanceA z) (DistanceA x) (DistanceA y) =
      constructor $ Point x y z
    --the actual point of intersection, provided they are not parallel.
    --have not yet checked for segment length. This will be done in next fx
    p1 = cpointSet (F1) (DistanceA 0) <$> xIntercept <*> yIntercept
    --if not parallel, then p1 else nothing.
    p1det =
      case topCoderAreTheParallel (BottomLeftLine b1 f1) (BackBottomLine b1' b4') of
        Left e -> Left e
        Right True -> Right Nothing
        Right False ->
          case p1 of
            Left e -> Left e
            Right cpoints -> Right $ Just cpoints
  in
    p1det
    
      


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

-- =================================================================================================
-- https://rosettacode.org/wiki/Find_the_intersection_of_two_lines#Haskell
--good for intersection of lines, but not segments
--need to see if point exists on a line

lineIntersection :: CornerPoints -> CornerPoints -> Either String (Maybe Point)
lineIntersection (BottomLeftLine (Point ax ay az) (Point bx by bz) ) (BackBottomLine (Point px py pz) (Point qx qy qz)) =
  let (pqDX, abDX) = (px - qx, ax - bx)
      (pqDY, abDY) = (py - qy, ay - by)
      determinant = abDX * pqDY - abDY * pqDX
      f pq ab =
        ((((ax * by) - (ay * bx)) * pq) - 
         (((px * qy) - (py * qx)) * ab)) /
        determinant
  in case determinant of
       0 -> Right Nothing
       _ -> Right $ Just (Point (f pqDX abDX) (f pqDY abDY) 0)


segmentIntersection :: CornerPoints -> --advancingCpoint
                            CornerPoints -> --perimeter
                            Either String Bool
segmentIntersection (BottomLeftLine b1 f1 ) (BackBottomLine b1' b4) =
  case lineIntersection (BottomLeftLine b1 f1) (BackBottomLine b1' b4) of
    Left e -> Left $ "Geometry.Intercept.segmentIntersection got error from lineIntersection: " ++ e
    Right Nothing -> Right False --Right Nothing. I no longer return a Maybe
    Right (Just pointOfIntersection) ->
      let
        advancingCpointSegmentIntersectsPerimeterLine ::  Either String Bool --add Either String once converted to bool
        advancingCpointSegmentIntersectsPerimeterLine =
          let
            pointingInCorrectDirectionLine :: Either String Bool
            pointingInCorrectDirectionLine =
              case getOrderingA <$> calculateDistanceA  f1 pointOfIntersection   <*> --length of B to intersection
                                   calculateDistanceA b1 pointOfIntersection --A to intersection,
              of
                Right GT -> Right False
                Right LT -> Right True
                  
                Right EQ -> Right True --not sure about this one. EQ would make advancingCpoint a Point
                Left e -> Left $ "Geometry.Intercept.segmentIntersection.advancingCpointSegmentIntersectsPerimeterLine.pointingInCorrectDirectionLine had an error: " ++ e
          in
          case getOrderingA <$> calculateDistanceA  b1 f1   <*> --length of BLL
                               calculateDistanceA b1 pointOfIntersection --f1 of bll,
          of
            Right LT -> Right False
            Right GT -> --True
              pointingInCorrectDirectionLine
            Right EQ -> --True
              pointingInCorrectDirectionLine
            Left e   -> Left $ "Geometry.Intercept.segmentIntersection.advancingCpointSegmentIntersectsPerimeterLine had an error : " ++ e
        
        perimeterSegmentIntersectsAdvancingCpointLine :: Either String Bool --add either later
        perimeterSegmentIntersectsAdvancingCpointLine = --LT if not long enough
          let
            pointingInCorrectDirectionLine :: Either String Bool
            pointingInCorrectDirectionLine =
              case getOrderingA <$> calculateDistanceA  b4 pointOfIntersection   <*> --length of B to intersection
                                   calculateDistanceA b1' pointOfIntersection --A to intersection,
              of
                Right GT -> Right False
                Right LT -> Right True
                Right EQ -> Right True --not sure about this one. EQ would make advancingCpoint a Point
                Left e -> Left $ "Geometry.Intercept.segmentIntersection.perimeterSegmentIntersectsAdvancingCpointLine.pointingInCorrectDirectionLine had an error: " ++ e
          in
          case getOrderingA <$> calculateDistanceA  b1' b4   <*> --length of BLL
                           calculateDistanceA b1' pointOfIntersection --f1 of bll,
          of
            Right LT -> Right False
            Right GT -> --True
              pointingInCorrectDirectionLine
            Right EQ -> --True
              pointingInCorrectDirectionLine
            Left e -> Left $ "Geometry.Intercept.segmentIntersection.perimeterSegmentIntersectsAdvancingCpointLine had an error: " ++ e
        
        process :: Bool -> Bool -> Bool
        process False False = False
        process False _ = False
        process  _ False = False
        process _ _ = True
          
    
      in
        process <$> advancingCpointSegmentIntersectsPerimeterLine <*> perimeterSegmentIntersectsAdvancingCpointLine
       {-
       case (advancingCpointSegmentIntersectsPerimeterLine) && (perimeterSegmentIntersectsAdvancingCpointLine) of
         True -> Right $ Just True
         False -> Right $ Just False -}

{-before adding Either to internal fx's
segmentIntersection :: CornerPoints -> --advancingCpoint
                            CornerPoints -> --perimeter
                            Either String (Maybe Bool)
segmentIntersection (BottomLeftLine b1 f1 ) (BackBottomLine b1' b4) =
  case lineIntersection (BottomLeftLine b1 f1) (BackBottomLine b1' b4) of
    Left e -> Left $ "Geometry.Intercept.segmentIntersection got error from lineIntersection: " ++ e
    Right Nothing -> Right Nothing
    Right (Just pointOfIntersection) ->
      let
        advancingCpointSegmentIntersectsPerimeterLine ::  Bool --add Either String once converted to bool
        advancingCpointSegmentIntersectsPerimeterLine =
          let
            pointingInCorrectDirectionLine :: Bool
            pointingInCorrectDirectionLine =
              case getOrderingA <$> calculateDistanceA  f1 pointOfIntersection   <*> --length of B to intersection
                                   calculateDistanceA b1 pointOfIntersection --A to intersection,
              of
                Right GT -> False
                Right LT -> True
                  
                Right EQ -> True --not sure about this one. EQ would make advancingCpoint a Point
                  
          in
          case getOrderingA <$> calculateDistanceA  b1 f1   <*> --length of BLL
                               calculateDistanceA b1 pointOfIntersection --f1 of bll,
          of
            Right LT -> False
            Right GT -> --True
              pointingInCorrectDirectionLine
            Right EQ -> --True
              pointingInCorrectDirectionLine
            --Left e   -> Left E
        
        perimeterSegmentIntersectsAdvancingCpointLine :: Bool --add either later
        perimeterSegmentIntersectsAdvancingCpointLine = --LT if not long enough
          let
            pointingInCorrectDirectionLine :: Bool
            pointingInCorrectDirectionLine =
              case getOrderingA <$> calculateDistanceA  b4 pointOfIntersection   <*> --length of B to intersection
                                   calculateDistanceA b1' pointOfIntersection --A to intersection,
              of
                Right GT -> False
                Right LT -> True
                Right EQ -> True --not sure about this one. EQ would make advancingCpoint a Point
                  
          in
          case getOrderingA <$> calculateDistanceA  b1' b4   <*> --length of BLL
                           calculateDistanceA b1' pointOfIntersection --f1 of bll,
          of
            Right LT -> False
            Right GT -> --True
              pointingInCorrectDirectionLine
            Right EQ -> --True
              pointingInCorrectDirectionLine
            --Left e
        
   
      in
       case (advancingCpointSegmentIntersectsPerimeterLine) && (perimeterSegmentIntersectsAdvancingCpointLine) of
         True -> Right $ Just True
         False -> Right $ Just False

-}
{- |
If the new advancingCpoint does not intersect the perimeter, then it is legal.
If it does intersect, but on a vertice, then it is legal. Should be the perimeter from which the new advancing line it to be built.
If it does intersect, not on a vertice, thin it is illegal, as advancingCpoint is probably crossing this perimenter on it's way to another.

-}
legalIntersection :: CornerPoints -> --advancingCpoint may intersect 
                     CornerPoints -> --a perimeter cpoint
                     Either String (Bool) --with and erorr, or legally: no intersection, or intersection at a vertice
legalIntersection CornerPointsNothing _ = Right True
legalIntersection _ CornerPointsNothing = Right True
legalIntersection (BottomLeftLine b1 f1 ) (BackBottomLine b1' b4) =
  let
    
    
    hasNoIntersectionOrIsOnVertice :: CornerPoints -> -- perimeter
                        Maybe Point -> --point of intersection
                        Bool  -> --is pointOfIntersection in perimeter segment
                        Either String Bool
    hasNoIntersectionOrIsOnVertice  _ _ False = Right True
    hasNoIntersectionOrIsOnVertice  (BackBottomLine b1' b4) pointOfIntersection intersectsPerimeter =
        case pointOfIntersection of
          Nothing -> Right True
          Just pointOfIntersection ->
            Right $ (pointOfIntersection ==  b1') || (pointOfIntersection ==  b4)
      
  in 
    extractE $
     hasNoIntersectionOrIsOnVertice (BackBottomLine b1' b4) <$>
                                              lineIntersection (BottomLeftLine b1 f1 ) (BackBottomLine b1' b4)  <*>
                                              segmentIntersection (BottomLeftLine b1 f1 ) (BackBottomLine b1' b4)



legalIntersection advancingCpoint perimeter =
  Left $ "Geometry.Intercept.legalIntersection has missing or illegal pattern match for advancingCpoint: " ++ (cpointType advancingCpoint) ++ " and  perimeter: " ++ (cpointType perimeter)

perimetersContainIllegalIntersection :: [[CornerPoints]] -> CornerPoints -> Either String Bool
perimetersContainIllegalIntersection (p:perimeters) cpoint =
  let
    perimetersContainIllegalIntersection' :: [CornerPoints] -> CornerPoints -> Either String Bool
    perimetersContainIllegalIntersection' (p:perimeter) cpoint =
      case legalIntersection cpoint p of
        Left e -> Left e
        Right False -> perimetersContainIllegalIntersection' perimeter cpoint
        Right True -> Right True
    perimetersContainIllegalIntersection' ([]) _ = Right False

  in
    case perimetersContainIllegalIntersection' p cpoint of
      Left e -> Left e
      Right False ->
        perimetersContainIllegalIntersection perimeters cpoint
      Right True -> Right True

perimetersContainIllegalIntersection [] _ = Right False


--temp fx to look inside of segmentIntersection
segmentIntersectionBreakDown :: CornerPoints -> CornerPoints -> Either String Ordering
segmentIntersectionBreakDown (BottomLeftLine (Point ax ay az) (Point bx by bz) ) (BackBottomLine (Point px py pz) (Point qx qy qz)) =
  case lineIntersection (BottomLeftLine (Point ax ay az) (Point bx by bz) ) (BackBottomLine (Point px py pz) (Point qx qy qz)) of
    Right Nothing -> Left "got Nothing intersection"
    Right (Just (Point x y z)) ->
      let
        notLongEnoughToIntersect = --LT if not long enough
          getOrderingA <$> calculateDistanceA  (Point ax ay az) (Point bx by bz)   <*> --length of BLL
                           calculateDistanceA (Point ax ay az) (Point x y z) --f1 of bll,
        pointingInCorrectDirections =
          getOrderingA <$> calculateDistanceA  (Point bx by bz) (Point x y z)   <*> --length of B to intersection
                           calculateDistanceA (Point ax ay az) (Point x y z) --A to intersection,

        test =
          case notLongEnoughToIntersect of
            Right LT -> Just False
            Right EQ ->  
              case pointingInCorrectDirections of
                Right LT -> Just True
                --need to make sure that 2nd point is long enough
            Right GT ->  
              case pointingInCorrectDirections of
                Right LT -> Just True
                --need to make sure that 2nd point is long enough
      in
       --test --filler to compile
       notLongEnoughToIntersect


-- http://bit-player.org/wp-content/extras/bph-publications/BeautifulCode-2007-Hayes.pdf
--is this not intersection of lines again, without knowing about segements
  --Not even that, as it takes three point, and sees if they are on the same line. But::::

onTheLine :: Point -> Point -> Point -> Double
onTheLine (Point x1 y1 z1) (Point x2 y2 z2) (Point x3 y3 z3) =
  (x1 - x3) * (y2 - y3) - (x2 - x3) * (y1 - y3)
