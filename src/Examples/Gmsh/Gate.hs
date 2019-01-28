{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples.Gmsh.Gate() where
{-
This is the 1st attempt at generating GMSH script.
The goal is to use the cornerpoints system of building up shapes, and generating
the gmsh script that will turn this into points, lines, and surfaces.

ToDo:
Need a new version of Builder.Monad that:
Has state that allows gmsh id's (eg: Point(1) where 1 is id), to be generated.
This will be done with a [1..] for each type <point/line/surfacef>.

--------------------------------------------------------------------------------------------------------------------------
in gate.geo: what is a curve loop vs a Plane Surface.
-curve loop would be like a cornerP Face.
-Plane surface tells gmsh what to mesh

Will need to create a Map of each type <point/line> where:
key :: CornerPoints
value :: Int, the gmsh id.
Will this also need to be in state, or can I modify?
There is a mutable hashmap in hackage, but is a monad. Would possibly need to
use Extensible Effects, to use it in a builder monad, as is not part of mtl(I think).

gmsh script should be gen'd in a top down fashion.
Eg:
Create a CornerPoints Cube, which is to be the entire shape.
 -Extract each face, which will be turned into the gmsh curve loop.
  -Break down each face into lines, and store the surface as: key: int, val: [<FrontTop/...>Line]
   -Break down each line into points.
    -insert each point into a map, with it's gmsh id, if not already inserted.
    -insert each line into a map if not already inserted.
  -insert the surfaces into a map, with a [lines] that make them up.
Once everything is mapped:
-work through the points map, gen'g script.
-work through the lines map, gen'g script.
-work through the curve loops map, gen'g script.
-manually add the gmsh Plane Surface for now
-}

import CornerPoints.CornerPoints(CornerPoints(..), (+++))
import CornerPoints.Points(Point(..))
import CornerPoints.Transpose ( transposeZ,transposeX,transposeY)
import CornerPoints.FaceConversions(toB3, toFrontTopLine)
import CornerPoints.FaceExtraction(extractF2, extractF3)

import Control.Monad.Writer (Writer, tell, execWriter)
import qualified Control.Monad.State.Lazy as S
import qualified Data.Map as M
import qualified Data.Hashable as H
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
{-
Create the gmsh script Point fx string for a Point.
Used by gmshCPointString, to create string for each Point contained in current CornerPoint
-}
gmshPointString :: Point -> Int -> String
gmshPointString (Point x y z) num =
  "\nPoint(" ++ (show num) ++ ") = {"  ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ "};"

{-
Extract each Point and call gmshPointString.
-}
gmshCPointString :: CornerPoints -> Int -> String
gmshCPointString (B2 b2) num =
  gmshPointString (negate_Y_point b2) num

gmshCPointString (B3 b3) num =
  gmshPointString (negate_Y_point b3) num

gmshCPointString (BackTopLine b2 b3) num = do
  gmshPointString (negate_Y_point b2) num
  gmshPointString (negate_Y_point b3) $ num + 1

gmshCPointString (F2 f2) num =
  gmshPointString (negate_Y_point f2) num

gmshCPointString (F3 f3) num =
  gmshPointString (negate_Y_point f3) num

gmshCPointString (FrontTopLine p1 p2) num = do
  gmshPointString p1 num
  gmshPointString p2 $ num + 1

gmshLineString :: [String] -> Int ->Int -> Int -> String
gmshLineString string point1 point2 lineId =
  let
    {-
    listString = concat
      [case tt == (show initialCount) of
         True -> tt
         False -> "," ++ tt
       | string' <- string
       | tt <- map show [initialCount..]
       
      ]
    -}
    listString = (show point1) ++ "," ++ (show point2)
    wrapperString =
      "\nLine("
      ++ (show lineId)
      ++ ") = {"
      ++ listString
      ++
      "};"
  in
  wrapperString 

{-
keep this as a way to do Curve Loop as it will take multiple lines.
gmshLineString :: [String] -> Int -> String
gmshLineString string initialCount =
  let
    listString = concat
      [case tt == (show initialCount) of
         True -> tt
         False -> "," ++ tt
       | string' <- string
       | tt <- map show [initialCount..]
       
      ]
    wrapperString =
      "\nLine(1) = {"
      ++
      listString
      ++
      "};"
  in
  wrapperString 

-}

{-
Create a TopFace.
Break it down into Points.
Print out the gmsh points script by manually building up the string. Need to use Writer or State.
Make 2 lines from the points.
Print out the gmsh lines script by manually building up the string. Need to use Writer or State.
Everthing is done very manaully.

Tests out the breakDownToPoints function.
-}

manaullyOutput2LinesFromAFace :: IO ()
manaullyOutput2LinesFromAFace = do
  putStrLn "created by ChampCad Examples.Gmsh.Gate"
  putStrLn "SetFactory(\"OpenCASCADE\");"
  let
    topLeftPoint = Point 0 0 0
    gateWidth = 700
    frameWidth = 70
    
    topBackLine =
      let
        b2 = B2 topLeftPoint
        b3 = toB3 $ transposeX (+gateWidth) b2
      in
      b2 +++ b3
    frontTopLine =
      let
        movedFwd = toFrontTopLine $ transposeY (+frameWidth) topBackLine 
        movedInF2 = transposeX (+frameWidth) $ extractF2 movedFwd
        movedInF3 = transposeX (\val -> val - frameWidth) $ extractF3 movedFwd
      in
      movedInF2 +++ movedInF3
    
    
    --
    printablePoints =
             [gmshPointString point counter
              | counter <- [1..]
              | point <- concat $ map breakDownToPoints [topBackLine, frontTopLine]
             ]
    
    lineString = gmshLineString (take 2 printablePoints) 1 2 1
    lineString2 = gmshLineString (drop 2 printablePoints) 3 4 2
  --print the points
  putStrLn $ (concat printablePoints)
  --print the lines
  putStrLn $ lineString ++ lineString2


{-
Task:
Test using a Writer to create the gmsh output.
Only output the b2 and b3 points.
Just manually add the id numbers.

If I need to use State for the gmsh Ids, why use a writer when I can build the string in the State monad?
When I build the Id generator with State, I will test that idea out.
-}
test2PointsWithWriter :: Writer String ()
test2PointsWithWriter = do
  tell "\\\\ created by ChampCad Examples.Gmsh.Gate"
  tell "\nSetFactory(\"OpenCASCADE\");"

  let
    topLeftPoint = Point 0 0 0
    gateWidth = 700
    frameWidth = 70
    
    topBackLine =
      let
        b2 = B2 topLeftPoint
        b3 = toB3 $ transposeX (+gateWidth) b2
      in
      b2 +++ b3
    frontTopLine =
      let
        movedFwd = toFrontTopLine $ transposeY (+frameWidth) topBackLine 
        movedInF2 = transposeX (+frameWidth) $ extractF2 movedFwd
        movedInF3 = transposeX (\val -> val - frameWidth) $ extractF3 movedFwd
      in
      movedInF2 +++ movedInF3

  tell $ gmshPointString topLeftPoint 1
  tell $ gmshPointString (b3 topBackLine) 2

runTest2PointsWithWriter :: IO ()
runTest2PointsWithWriter = putStrLn $ execWriter test2PointsWithWriter

{-
Break down CornerPoints into [Point].
Known use:
Used to create list of all Points to be printed out, along with a number generator for the Point fx.
-}
breakDownToPoints :: CornerPoints -> [Point]
breakDownToPoints (B2 b2) = [b2]
breakDownToPoints (B3 b3) = [b3]
breakDownToPoints (BackTopLine b2 b3) = [b2,b3]
breakDownToPoints (FrontTopLine f2 f3) = [f2,f3]

--converts from ChampCad cartesion system to Gmsh cartesion system layout.
negate_Y_point :: Point -> Point
negate_Y_point (Point x y z) = Point x (negate y) z
{-do not need this as used negate_Y_point directly in the print printCPoint fx's.
negate_Y_cpoint :: CornerPoints -> CornerPoints

negate_Y_cpoint (B2 b2) = B2 $ negate_Y_point b2
negate_Y_cpoint (B3 b3) = B3 $ negate_Y_point b3

negate_Y_cpoint (F2 f2) = F2 $ negate_Y_point f2
negate_Y_cpoint (F3 f3) = F3 $ negate_Y_point f3

negate_Y_cpoint (BackTopLine b2 b3) =
  (B2 $ negate_Y_point b2) +++ (B3 $ negate_Y_point b3)


negate_Y_cpoint (FrontTopLine f2 f3) =
  (F2 $ negate_Y_point f2) +++ (F3 $ negate_Y_point f3)
-}


{-
Task:
create a ADT with 2 maps <Points/Lines> using state.
So far it is just Map Int Int
-now make it use a CornerPoints key, but will need to make it an instance or ORD.
 -Is there a better alt? Use Data.HashMap.Strict but 1st need to make Point an instance of Hashable
-}
data Test1 = Test1 {pointsMap :: M.Map Int Int, val12 :: M.Map Int Int}
  deriving (Show, Eq)

pointsMapWithStateWithMap :: S.State  Test1 Int
pointsMapWithStateWithMap = do
  let mapper1 :: Test1 -> Test1
      mapper1 state =
        state {pointsMap= (M.insert 3 4 $ pointsMap state)}
      mapper2 :: Test1 -> Test1
      mapper2 (Test1 value1 value2) =
        Test1 value1 (M.insert 5 6 value2) 
  currState1 <- S.get
  S.put $ mapper1 currState1
  S.modify $ mapper2
  return 1

runPointsMapWithStateWithMap =
  S.execState  pointsMapWithStateWithMap $ Test1 M.empty M.empty
 {-
tried to use a Point for the key
data Test1 = Test1 {pointsMap :: M.Map Point Int, val12 :: M.Map Int Int}
  deriving (Show, Eq)

pointsMapWithStateWithMap :: S.State  Test1 Int
pointsMapWithStateWithMap = do
  let mapper1 :: Test1 -> Test1
      mapper1 state =
        state {pointsMap= (M.insert (Point 1 1 1) 4 $ pointsMap state)}
      mapper2 :: Test1 -> Test1
      mapper2 (Test1 value1 value2) =
        Test1 value1 (M.insert 5 6 value2) 
  currState1 <- S.get
  S.put $ mapper1 currState1
  S.modify $ mapper2
  return 1

------------------------------------------------------------------------
data Test1 = Test1 {pointsMap :: M.Map Int Int, val12 :: M.Map Int Int}
  deriving (Show, Eq)



-}
--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------
--make a points hash table for storing points info.
{-
pointSalt = 1

--make Point a instance of Hashable so it can be  used with Data.HashMap.Strict
instance H.Hashable Point where
    hashWithSalt s (Point x y z) =
        s `H.hashWithSalt`
        x `H.hashWithSalt`
        y `H.hashWithSalt` z

--test: use  H.hashWithSalt on Point
hashMyPoint s point =
  --print $ show $ H.hashWithSalt s $ Point 1 2 3 
  H.hashWithSalt s point  

--test: insert the hashable point into a hash map.
showHashMyPoint = do
  let
    val = 1
      --the val to store in the hash map
  print $ show $ HM.insert (hashMyPoint pointSalt $ Point 1 2 3) val HM.empty

--test: insert a hashed Point into a hash map
insertHashedPointIntoHashMap = do
  let
    val = 1
      --the val to store in the hash map
  
  HM.insert (hashMyPoint pointSalt $ Point 1 2 3) val HM.empty
-}
--------------------------------------------------------------------------
--------------------------------------------------------------------------
--------------------------------------------------------------------------
----now make CornerPoints a hash
{-
cPointSalt = 1

instance H.Hashable CornerPoints where
  hashWithSalt s (B1 point) =
    s `H.hashWithSalt` point

hashMyCPoint s point =
  --print $ show $ H.hashWithSalt s $ Point 1 2 3 
  H.hashWithSalt s point

showHashMyCPoint =
  print $ show $ hashMyCPoint cPointSalt $ Point 1 2 3

insertMyCPoint = do
  let val = 1
        --the value to store in the hashMap
  
  print $ show $ HM.insert (hashMyCPoint cPointSalt $ B1 $ Point 1 2 3) val HM.empty
-}
