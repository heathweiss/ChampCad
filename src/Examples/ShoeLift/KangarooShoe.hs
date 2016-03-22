module Examples.ShoeLift.KangarooShoe where
{-
The runners from Wallmart, that use the old treads from his black runners.

Made a very small toe, which allows the runner to be light and flexible but:
it gives no support in the center, which caused him problems. 
-}

import Test.HUnit

import CornerPoints.Radius(Radius(..), buildSymmetricalRadius)
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces)
import CornerPoints.Points(Point(..))
import CornerPoints.Create(Angle(..), flatXSlope, flatYSlope, Slope(..))
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|), DegreeRange(..))



import CornerPoints.CornerPoints((|@+++#@|), (|+++|), CornerPoints(..), (+++))
import CornerPoints.Transpose(transposeZ, transposeY)
import CornerPoints.FaceExtraction(extractTopFace, extractBottomFace, extractFrontFace)
import CornerPoints.FaceConversions(upperFaceFromLowerFace, backFaceFromFrontFace )
import CornerPoints.Degree(Degree(..))
import CornerPoints.Transposable(transpose)

import Control.Lens

import Builder.List((&@~+++@), (&@~+++#@), (||@~+++^||), newCornerPointsWith10DegreesBuilder)
import qualified Builder.Sequence as S (newCornerPointsWith10DegreesBuilder, (||@~+++^||), (@~+++#@|>), (@~+++@|>))

import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..), (+++^))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import Stl.StlCornerPointsWithDegrees(FacesWithRange(..))

import Primitives.Cylindrical(cylinderSolidNoSlopeSquaredOff, cylinderWallsNoSlope, cylinderWallsVariableRadiusNoSlope,
                              cylinderWallsVariableThicknessNoSlope, cylinderWallsVariableThicknessSloped, cylinderSolidVariableRadiusVariableTopSlope )

--make signatures more readable
type Thickness = Double
type Height    = Double

origin = Point 0 0 0
angles = map Angle [0,10..]

-- ================================================== center piece =========================================
{-
Left a large central gap between the toe and the heel. Dom does not like this, so need to add in a central filler between the toe and heel.
Consists of:
Slope: Attaches to the shoe with a 15 degree slope. Goes to a common squared riser shape.

Riser: a common riser squared shape.

Tread: Converts from the tread to the common riser.
-}

--radius where center filler meets the shoe.
centerTopRadius =
   map Radius
   [35.4, --0
    35.4, --10
    37.0, --20
    39.5, --30
    43.1, --40
    39.3, --50
    37.0, --60
    36.4, --70
    36.1, --80
    37.8, --90
    39.9, --100
    43.3, --110
    48.1, --120
    54.3, --130
    46.4, --140
    41.4, --150
    38.5, --160
    37.0, --170
    36.6, --180
    37.4, --190
    39.8, --200
    43.7, --210
    49.3, --220
    59.7, --230
    51.4, --240
    44.3, --250
    39.4, --260
    36.3, --270
    34.7, --280
    33.9, --290
    34.7, --300
    38.2, --310
    46.2, --320
    42.5, --330
    38.7, --340
    36.4, --350
    35.4--360
   ]

{-Works as both the sloped top piece, and the flat main riser section-}
writeCenterTopSlope = writeStlToFile $ newStlShape "kangaroo center" $
                       [FacesBottomFrontTop | x <- [1..]]
                       |+++^|
                       cylinderSolidVariableRadiusVariableTopSlope centerTopRadius origin angles [flatXSlope | x <- [1..]] [flatYSlope | x <- [1..]] (30.0::Height)
                       --the sloped section
                       --cylinderSolidVariableRadiusVariableTopSlope centerTopRadius origin angles [flatXSlope | x <- [1..]] (map NegYSlope [15,15..]) (20.0::Height)


{-The side plates for gluing/attaching the riser.-}
centerSideAttachments =
  let btmFace = BottomFace {b1 = Point 0 0 0,
                            b4 = Point 45 0 0,
                            f1 = Point 0 70 0,
                            f4 = Point 23 70 0
                           }
      topFace = transposeZ (+1) $ upperFaceFromLowerFace btmFace
  in
      writeStlToFile $ newStlShape "kangaroo center sides" $
      FacesAll
      +++^
      (btmFace +++ topFace)
  
-- =============================================== toe tread ==============================================


{-This is the top to the recycled tread. Will attach to shoe via an adaptor. Will need a wrapper around the top
of it, to cover the wire used for attaching.-}

{-This is the top of an existing tread from a previous shoe.-}
toeOfTreadRadius =
  map Radius
   [37.6, --0
    37.6, --10
    36.3, --20
    34.5, --30
    33.4, --40
    32.2, --50
    31.8, --60
    31.1, --70
    31.3, --80
    32.8, --90
    34.9, --100
    37.8, --110
    42.1, --120
    49.2, --130
    42.3, --140
    37.8, --150
    34.7, --160
    33.1, --170
    33.0, --180
    33.5, --190
    35.6, --200
    38.9, --210
    44.6, --220
    54.5, --230
    45.7, --240
    39.9, --250
    36.0, --260
    33.5, --270
    32.4, --280
    31.1, --290
    31.0, --300
    31.8, --310
    33.5, --320
    34.7, --330
    35.8, --340
    37.6, --350
    37.6 --360
   ]
   
toeOfShoeRadius = map Radius
  [ 36.4,--0
    36.4 ,--10
    36.1 ,--20
    35.5,--30
    34.1,--40
    34.1,--50
    34.8,--60
    35.8,--70
    36.7,--80
    38.6,--90
    41.1,--100
    44.4,--110
    49.3,--120
    56.0,--130
    45.8,--140
    39.8,--150
    36.4,--160
    34.4,--170
    33.5,--180
    34.0,--190
    35.6,--200
    38.6,--210
    43.9,--220
    52.8,--230
    47.8,--240
    44.1,--250
    40.6,--260
    38.8,--270
    38.2,--280
    37.1,--290
    36.8,--300
    36.8,--310
    36.5,--320
    36.5,--330
    36.5,--340
    36.5,--350
    36.4--360
  ]

toeOfShoeTriangles =
 ((newCornerPointsWith10DegreesBuilder $ --riser meets original lift from former shoe, 5 mm high
   (createTopFaces (Point 0 0 5) toeOfTreadRadius angles flatXSlope flatYSlope)
   |+++|
   (createBottomFaces (Point 0 0 0) toeOfTreadRadius angles flatXSlope flatYSlope)
  )
  &@~+++@
  (createTopFaces (Point 0 0 10) toeOfShoeRadius angles flatXSlope flatYSlope) --adapt to the tread of shoe, 5 mm high
  &@~+++#@
  ((transposeZ (+5))  . extractTopFace) --tread of the shoe
 )
 ||@~+++^||
 [ [(FacesWithRange FacesFrontTop (DegreeRange 0 360))],   --riser meets shoe
   [(FacesWithRange FaceFront (DegreeRange 0 360))],       --riser to shoe converstion
   [(FacesWithRange FacesBottomFront (DegreeRange 0 360))] --original lift
 ]

writeToe =  writeStlToFile $ newStlShape "kangaroo toe" toeOfShoeTriangles

-- ======================================== toe wire cover ===============================
{-This will a thin wall to wrap around the bottom of the shoe tread and top of the riser, to cover the wire,
and give extra vertical glue area.
-}

{-The front of the shoe slopes forward, so need a top outward slope for the front of the cover, for section on shoe.
The sides do not need to slope outwards.-}
toeWireCoverShoeRadius = zipWith (transpose)
                          (concat
                            [[(+2)| x <- [1..4]],
                             [(+0)| x <- [5..32]],
                             [(+2)| x <- [1..]]
                            ]
                          )
                          toeOfShoeRadius

toeWireCoverTriangles =
 ((newCornerPointsWith10DegreesBuilder $
   cylinderWallsVariableRadiusNoSlope toeOfShoeRadius (1.0::Thickness)  (Point 0 0 0) angles (5::Height)
  )
  &@~+++@
  (map (extractTopFace) (cylinderWallsVariableRadiusNoSlope toeWireCoverShoeRadius (1.0::Thickness)  (Point 0 0 5) angles (13::Height)))
 )
 ||@~+++^||
 [ [(FacesWithRange FacesBackFrontTop (DegreeRange 0 80)),       --
    (FacesWithRange FacesBackFrontLeftTop (DegreeRange 80 90)),     --
    (FacesWithRange FacesBackFrontRightTop (DegreeRange 270 280)),  --
    (FacesWithRange FacesBackFrontTop (DegreeRange 280 360))
   ],
   [(FacesWithRange FacesBackBottomFront (DegreeRange 0 80)),       --
    (FacesWithRange FacesBackBottomFrontLeft (DegreeRange 80 90)),     --
    (FacesWithRange FacesBackBottomFrontRight (DegreeRange 270 280)),  --
    (FacesWithRange FacesBackBottomFront (DegreeRange 280 360))
   ]     -- 
 ]

writeToeWireCover = writeStlToFile $ newStlShape "kangaroo toe wire cover" toeWireCoverTriangles

-- ============================================== ankle brace ===========================================
braceRadius = buildSymmetricalRadius
  [ 35.7,--0
    35.2,--10
    35.2,--20
    33.2,--30
    32.4,--40
    31.8,--50
    32.2,--60
    32.4,--70
    33.5,--80
    35.0,--90
    37.3,--100
    40.9,--110
    44.8,--120
    48.7,--130
    --beyond here will not be printed
    50.0,--140
    50.0,--150
    50.0,--160
    50.0--170
  ]
  50.0 --180

--The heelWireCoverOuterRadius + 3mm,  which will be the riser from heel to top brace
ankleBraceRiserOuterRadius = map (transpose (+3)) heelWireCoverOuterRadius

ankleBraceCubes =
  --the wrapper around the ankle
  ( S.newCornerPointsWith10DegreesBuilder $
    cylinderWallsVariableRadiusNoSlope braceRadius (3::Thickness) (Point 0 0 70) angles (30::Height)                                 
  )
  S.@~+++@|>
  --down to the top of the heel wire cover 
  --(map ((transposeY (+(-20))) . extractBottomFace) (cylinderWallsVariableThicknessSloped heelOfShoeRadius heelWireCoverOuterRadius angles flatXSlope (NegYSlope 15)  (30::Height)))
  (map ((transposeY (+(-20))) . extractBottomFace) (cylinderWallsVariableThicknessNoSlope heelWireCoverOuterRadius ankleBraceRiserOuterRadius angles (Point 0 0 0) (53::Height)))
  --down to the bottom of the heel wire cover
  S.@~+++#@|>
  ( (transposeZ (+(-40))) .extractBottomFace )

ancleBraceTriangles =
  ankleBraceCubes
  S.||@~+++^||
  [ --ankle wrapper
    [(FacesWithRange FacesBackFrontTop (DegreeRange 0 20)),
    (FacesWithRange FacesBackBottomFrontTop (DegreeRange 20 120)),
    (FacesWithRange FacesBackBottomFrontLeftTop (DegreeRange 120 130)),
    (FacesWithRange FacesBackBottomFrontRightTop (DegreeRange 230 240)),
    (FacesWithRange FacesBackBottomFrontTop (DegreeRange 240 340)),
    (FacesWithRange  FacesBackFrontTop (DegreeRange 340 360))
   ],
    --riser between heel and wrapper
   [ (FacesWithRange FacesBackFront (DegreeRange 0 10)),
      (FacesWithRange FacesBackFrontLeft (DegreeRange 10 20)),
      (FacesWithRange FacesBackFrontRight (DegreeRange 340 350)),
      (FacesWithRange FacesBackFront (DegreeRange 350 360))
   ],
   --wire cover
   [ (FacesWithRange FacesBackBottomFront (DegreeRange 0 10)),
      (FacesWithRange FacesBackBottomFrontLeft (DegreeRange 10 20)),
      (FacesWithRange FacesBackBottomFrontRight (DegreeRange 340 350)),
      (FacesWithRange FacesBackBottomFront (DegreeRange 350 360))
    ]
  ]

writeAnkleBrace = writeStlToFile $ newStlShape "kangaroo heel ankle brace" ancleBraceTriangles


{- ====================================== heel of shoe wire cover===========================================
A wrapper to cover the wire where riser meets shoe, and provide extra vertical glue area.
Has the back modified for attachment to the ankle brace.-}

heelWireCoverOuterRadius =
  let transposeBy =
        concat
        [ [(+2) | x <- [0,1,2]],
          [(+1) | x <- [3..33]],
          [(+2) | x <- [34,35,36]]
        ]
  in  zipWith transpose  transposeBy  heelOfShoeRadius
{-  
backFaces = map (backFaceFromFrontFace . extractFrontFace) (createBottomFaces (Point 0 0 0) heelOfShoeRadius angles flatXSlope flatYSlope)
frontFaces =  map (extractFrontFace) (createBottomFaces (Point 0 0 0) heelOfShoeRadius angles flatXSlope flatYSlope)
btmFaces = backFaces |+++| frontFaces  
-}

heelWireCoverCubes =
  (S.newCornerPointsWith10DegreesBuilder $
    cylinderWallsVariableThicknessNoSlope heelOfShoeRadius heelWireCoverOuterRadius angles (Point 0 0 0) (5::Height)
  )
  S.@~+++@|>
  (map extractTopFace (cylinderWallsVariableThicknessSloped heelOfShoeRadius heelWireCoverOuterRadius angles (Point 0 0 0) flatXSlope (NegYSlope 15)  (30::Height)))
  

heelWireCoverTriangles =
  heelWireCoverCubes
  S.||@~+++^||
  [ [ (FacesWithRange FacesBackBottomFront (DegreeRange 0 130)),
      (FacesWithRange FacesBackBottomFrontLeft (DegreeRange 130 140)),
      (FacesWithRange FacesNada (DegreeRange 140 220)),
      (FacesWithRange FacesBackBottomFrontRight (DegreeRange 220 230)),
      (FacesWithRange FacesBackBottomFront (DegreeRange 230 360))
    ],
    [ (FacesWithRange FacesBackFrontTop (DegreeRange 0 130)),
      (FacesWithRange FacesBackFrontLeftTop (DegreeRange 130 140)),
      (FacesWithRange FacesNada (DegreeRange 140 220)),
      (FacesWithRange FacesBackFrontRightTop (DegreeRange 220 230)),
      (FacesWithRange FacesBackFrontTop (DegreeRange 230 360))
    ]
  ]

writeHeelWireCover = writeStlToFile $ newStlShape "kangaroo heel wire cover" heelWireCoverTriangles
-- ===================================================== heel of shoe =====================================================
{-The top is the shape of the heel of the shoe.
The bottom is a standard riser shape of squaredOff Radius 20 ^4-}
heelOfShoeRadius = buildSymmetricalRadius
   [ 35.8,--0
     35.3,--10
     35.0,--20
     35.3,--30
     34.6,--40
     33.7,--50
     33.6,--60
     32.2,--70
     31.4,--80
     31.3,--90
     32.0,--100
     33.4,--110
     35.8,--120
     39.7,--130
     46.3,--140
     41.9,--150
     38.2,--160
     36.0  --170
   ]
   35.1 --180

                 

heelOfShoeTriangles =         (
                               ( newCornerPointsWith10DegreesBuilder $
                                    (createTopFaces (Point 0 0 10) heelOfShoeRadius angles flatXSlope (NegYSlope 15))
                                    |+++|
                                    (createBottomFaces (Point 0 0 0) heelOfShoeRadius angles flatXSlope flatYSlope)
                               )
                                
                                &@~+++#@
                                ((transposeZ(+(-5))).extractBottomFace)
                                &@~+++@
                                ((map (extractBottomFace) $ cylinderSolidNoSlopeSquaredOff (Radius 25) (Point 0 0 (-10)) angles 0 4) )
                                &@~+++#@
                                (transposeZ(+(-10)) . extractBottomFace)
                                
                              )
                              ||@~+++^||
                              {-[CubesWithDegrees] is made from bottom up, so align [FacesWithRange] same way.-}
                              [  [ (FacesWithRange FacesBottomFront (DegreeRange 0 360))],
                                 [ (FacesWithRange FaceFront (DegreeRange 0 360))],
                                 [ (FacesWithRange FacesFrontTop (DegreeRange 0 360))]{-,
                                 [ (FacesWithRange FacesFrontTop    (DegreeRange 0 360))]-}
                              ]


writeHeelToShoe = writeStlToFile $ newStlShape "kangaroo heel to shoe" heelOfShoeTriangles



-- =================================================== heel tread ===========================================
{-The top is a standard riser shape of squaredOff Radius 20 ^4.
The bottom is the shape of the top of the recyled heel from his cowboy boot.-}
heelOfTreadRadius =
  buildSymmetricalRadius
   [ 36.9, --0
     36.7, --10
     36.6, --20
     36.1, --30
     35.2, --40
     33.8, --50
     31.5, --60
     29.9, --70
     27.7, --80
     27.5, --90
     27.7, --100
     28.5, --110
     30.7, --120
     34.1, --130
     30.4, --140
     26.4, --150
     23.8, --160
     22.5  --170
   ]
   22.2 --180

heelOfTreadTriangles  =
      (
       ( newCornerPointsWith10DegreesBuilder $
          (createBottomFaces (Point 0 0 0) heelOfTreadRadius angles flatXSlope flatYSlope)  
          |@+++#@|
          ((transposeZ(+5)). upperFaceFromLowerFace)
       )
      &@~+++@
     ((map (extractTopFace) $ cylinderSolidNoSlopeSquaredOff (Radius 25) (Point 0 (-7) 10) angles 0 4) )
     &@~+++#@
     (transposeZ(+5) . extractTopFace)
    )
    ||@~+++^||
    [ [ (FacesWithRange FacesFrontTop (DegreeRange 0 360))],
      [ (FacesWithRange FaceFront (DegreeRange 0 360))],
      [ (FacesWithRange FacesBottomFront (DegreeRange 0 360))]
       
    ]        

writeHeelOfTread = writeStlToFile $ newStlShape "kangaroo heel tread" heelOfTreadTriangles




-- ==================================================== tests ==================================================
kangarooShoeTestDo = do
  {- heelOfShoeCubesWithDegrees was merged into triangles'
  let seeBottomFacesWithDegrees = TestCase $ assertEqual
        "seeBottomFacesWithDegrees"
        []
  --runTestTT seeBottomFacesWithDegrees
  

  
  let seeLenghtOfTopLayerOfCubesList = TestCase $ assertEqual
        "seeLenghtOfTopLayerOfCubesList"
        36
        (length $ head heelOfShoeCubesWithDegrees)
  runTestTT seeLenghtOfTopLayerOfCubesList

  let seeLenghtOfLastLayerOfCubesList = TestCase $ assertEqual
        "seeLenghtOfLastLayerOfCubesList"
        36
        (length $ last heelOfShoeCubesWithDegrees)
  runTestTT seeLenghtOfLastLayerOfCubesList
  -}
  
  
  
  let seeRadiusLength = TestCase $ assertEqual
        "seeRadiusLength"
        37 --not 36 because starts at 0 instead of 1
        (length heelOfShoeRadius)
  runTestTT seeRadiusLength
   
  let seeRadius = TestCase $ assertEqual
        "seeRadius"
        [Radius {radius = 35.8},Radius {radius = 35.3},Radius {radius = 35.0},Radius {radius = 35.3},Radius {radius = 34.6},Radius {radius = 33.7},Radius {radius = 33.6},Radius {radius = 32.2},Radius {radius = 31.4},Radius {radius = 31.3},Radius {radius = 32.0},Radius {radius = 33.4},Radius {radius = 35.8},Radius {radius = 39.7},Radius {radius = 46.3},Radius {radius = 41.9},Radius {radius = 38.2},Radius {radius = 36.0},Radius {radius = 35.1},Radius {radius = 36.0},Radius {radius = 38.2},Radius {radius = 41.9},Radius {radius = 46.3},Radius {radius = 39.7},Radius {radius = 35.8},Radius {radius = 33.4},Radius {radius = 32.0},Radius {radius = 31.3},Radius {radius = 31.4},Radius {radius = 32.2},Radius {radius = 33.6},Radius {radius = 33.7},Radius {radius = 34.6},Radius {radius = 35.3},Radius {radius = 35.0},Radius {radius = 35.3},Radius {radius = 35.8}]
        (toeWireCoverShoeRadius)
  runTestTT seeRadius































































































































































































     


  
