{-# LANGUAGE ParallelListComp #-}
module CornerPoints.VerticalFaces(
  createRightFaces, createRightFacesNoSlope, createVerticalFaces,
  createLeftFaces, createLeftFacesNoSlope,
  createHorizontallyAlignedCubes, createHorizontallyAlignedCubesNoSlope,
  createLeftFacesMultiColumns, createLeftFacesMultiColumnsNoSlope, createVerticalWalls,
  TransposeFactor(..)) where
import CornerPoints.Create(Slope(..), Origin(..), createCornerPoint, Angle(..), flatXSlope, flatYSlope)
import CornerPoints.CornerPoints(CornerPoints(..), (+++>), (+++), (|+++|))
import CornerPoints.Transpose (transposeZ)
import CornerPoints.Points(Point(..))
import CornerPoints.Radius(Radius(..), SingleDegreeRadii(..), Degree(..), MultiDegreeRadii(..))
import TypeClasses.Transposable( TransposeLength, transpose)
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace)
import CornerPoints.FaceConversions(backFaceFromFrontFace)                           



----------------------------------------- create left/right faces from Scan datatype------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{----------------------------------------- create left/right faces, no Scan datatype----------------------------------------------------
This is a base that gets called by createLeftFaces and createRightFaces, with just a different set of CornerPoints constructors.
It uses [[Radius]] and [degrees], instead of the new Scan datatype. Once the Scan datatype can be used, this will be removed.

Build [Left/RightFaces], from an array of Radius that runs downwards along
a single degree. This is the way that the scanner process the images, and gives them to TriCad.
If the data is supplied in an upwards direction, reverse the data, then use this same function, instead of
building a separate function for that purpose.

The vertical distance of each height, is the same for all pixels. Therefore, the height of each pixel
is a factor of location in the array. So to calculate the z_axis(height), an array is passed in which
will be zipped with the RightFaces after they have been created, using TransposeZ.
It would be more efficient to calculate it as the Faces are made up, but then would not be able to re-use
the createCornerPoint function.


Known uses:
Scanning
Scanning is done vertically, so it would be best to build the model that way, instead of transforing the data
to fit the horizontal model supplied by 'createBottomFaces'/'createTopFaces'

Create a set of right faces, which will be used as the initial faces, to which all the subsequent left
faces will be added via +++>. This is used to build both right/left faces.

Create in a top down direction, as that is the way the openCV data is supplied.

------------ given -----------
topOrigin:
-Point that gives the inner origin for the topmost face caculated. This is also the first Radius.

degree:
-The current degree on xy axis. Should be 0.

xSlope:
-x-axis slope on the top of the shape

ySlope:
-y-axis slope on the top of the shape

zTransposeFactor:
-An array of the heights(z_axis value) associated with each face. This value will be subtracted
 from the topOrigin z_axis, and so must be an ascending value. Can/should be infinite.

inRadius:
-The array of Radius, which is the value read in from the file, as was calculated openCV.
 This is the location of the target value, in pixels. It needs to be translated into a distance.

---------returns ----------
An array of LeftFace or RightFace depending on data constructors passed in.
-}
createVerticalFaces :: Origin -> SingleDegreeRadii -> Slope -> Slope -> [TransposeFactor] -> (Point-> CornerPoints) ->
                       (Point-> CornerPoints) -> (Point-> CornerPoints) -> (Point-> CornerPoints) -> [CornerPoints]
createVerticalFaces origin (SingleDegreeRadii degree' radii') xSlope ySlope zTransposeFactor topFrontConstructor topBackConstructor
                            btmFrontConstructor btmBackConstructor =
  
  --zipWith  (\x y -> transposeZ ((-x)+) y  ) --negating x causes the z_axis to decrease from the top, as it should.
  -- zTransposeFactor --this should be an infinit of the height for each pixel, measured from topOrigin.
   let topFrontPoint =
         (createCornerPoint
           (topFrontConstructor)
           origin
           (head $ radii') 
           (Angle degree')
           xSlope
           ySlope
         )
       topBackPoint =  topBackConstructor origin

       topLine = topFrontPoint +++ topBackPoint

       bottomLines =
         [(createCornerPoint
            (btmFrontConstructor)
            (transposeZ (+(-currZVal)) origin)  --topOrigin
            currRadius
            (Angle degree')
            xSlope
            ySlope
          ) 
          +++
          btmBackConstructor (transposeZ (+(-currZVal)) origin)  --topOrigin
            | currRadius <- tail $ radii'
            | currZVal <-  zTransposeFactor
         ]
       
   in  
       topLine
       +++>
       bottomLines
   

-- |Create a column of RightFace
createRightFaces :: Origin -> SingleDegreeRadii -> Slope -> Slope -> [TransposeFactor] -> [CornerPoints]
createRightFaces origin singleDegreeRadii xSlope ySlope zTransposeFactor  =
  createVerticalFaces origin singleDegreeRadii xSlope ySlope zTransposeFactor (F3) (B3) (F4) (B4)

createRightFacesNoSlope :: Origin -> SingleDegreeRadii -> [TransposeFactor] -> [CornerPoints]
createRightFacesNoSlope origin singleDegreeRadii zTransposeFactor  =
  createVerticalFaces origin singleDegreeRadii flatXSlope flatYSlope zTransposeFactor (F3) (B3) (F4) (B4)


-- |Create a column of LeftFace 
createLeftFaces :: Origin -> SingleDegreeRadii -> Slope -> Slope -> [TransposeFactor] -> [CornerPoints]
createLeftFaces origin singleDegreeRadii xSlope ySlope zTransposeFactor  =
  createVerticalFaces origin singleDegreeRadii xSlope ySlope zTransposeFactor (F2) (B2) (F1) (B1)

createLeftFacesNoSlope :: Origin -> SingleDegreeRadii -> [TransposeFactor] -> [CornerPoints]
createLeftFacesNoSlope origin singleDegreeRadii zTransposeFactor  =
  createVerticalFaces origin singleDegreeRadii flatXSlope flatYSlope zTransposeFactor (F2) (B2) (F1) (B1)


{-
Can already create LeftFace, problem is to create an array of them from the vertical [Radius].
Cannot just map over them with createLeftFaces, because the degree is changing from column to column.
For this reason, will have to use recursion.
-}
createLeftFacesMultiColumns ::  Origin -> [SingleDegreeRadii] -> Slope -> Slope -> [TransposeFactor] -> [[CornerPoints]]
createLeftFacesMultiColumns _ [] _ _ _ = []
createLeftFacesMultiColumns topOrigin (d:ds) xSlope ySlope zTransposeFactor =
  (createLeftFaces topOrigin d xSlope ySlope zTransposeFactor ) :
    (createLeftFacesMultiColumns topOrigin ds xSlope ySlope zTransposeFactor)

createLeftFacesMultiColumnsNoSlope ::  Origin -> [SingleDegreeRadii] -> [TransposeFactor] -> [[CornerPoints]]
createLeftFacesMultiColumnsNoSlope _ [] _ = []
createLeftFacesMultiColumnsNoSlope topOrigin (d:ds) zTransposeFactor =
  (createLeftFacesNoSlope topOrigin d zTransposeFactor ) :
    (createLeftFacesMultiColumnsNoSlope topOrigin ds zTransposeFactor)




{- |Join a [RightFace] to [[LeftFace]]
    Results in [[CornerPoints]] where each inner list represents horizontal layer..

    Used to create cubes from all the faces generated from a MultiDegreeRadii.
-}

  
{-

Normally: RightFace +++> [LeftFaces], instead of  [RightFace] +++> [[LeftFace]],  so need recursion to work through the extra level of lists.
-}

createHorizontallyAlignedCubes :: [CornerPoints] -> [[CornerPoints]] -> [[CornerPoints]]
createHorizontallyAlignedCubes ([]) _ = []
createHorizontallyAlignedCubes (x:xs) (ys) =
  let headOfLeftFaces = map (head) ys
  in (x +++> headOfLeftFaces) : (createHorizontallyAlignedCubes xs (map (tail) ys) )


createHorizontallyAlignedCubesNoSlope :: Origin -> MultiDegreeRadii -> [TransposeFactor] -> [[CornerPoints]]
createHorizontallyAlignedCubesNoSlope origin (MultiDegreeRadii name' degrees') transposeFactors =
  let leftFaces = createLeftFacesMultiColumnsNoSlope origin (tail degrees') transposeFactors
      rightFaces = createRightFacesNoSlope origin (head degrees' )  transposeFactors
  in  createHorizontallyAlignedCubes rightFaces leftFaces

{-
create version that takes [[CornerPoints]]
-}
createHorizontallyAlignedCubesNoSlope' :: Origin -> [SingleDegreeRadii] -> [TransposeFactor] -> [[CornerPoints]]
createHorizontallyAlignedCubesNoSlope' origin cpoints transposeFactors =
  let leftFaces = createLeftFacesMultiColumnsNoSlope origin (tail cpoints) transposeFactors
      rightFaces = createRightFacesNoSlope origin (head cpoints )  transposeFactors
  in  createHorizontallyAlignedCubes rightFaces leftFaces

{- |Create a radial shape with walls based on adding together, an inner and outer [SingleDegreeRadii]
The resulting [[CornerPoints]] are horizontal rings(x y axis) of [CornerPoints] which start at the top(highest z axis).
-}
createVerticalWalls ::  [SingleDegreeRadii] ->   [SingleDegreeRadii] ->      Origin -> [TransposeFactor] -> [[CornerPoints]]
createVerticalWalls     innerSDR    outerSDR origin    transposeFactors =
  zipWith (|+++|)
          (map (map (extractFrontFace)) (createHorizontallyAlignedCubesNoSlope' origin outerSDR transposeFactors))
          (map (map(backFaceFromFrontFace . extractFrontFace)) (createHorizontallyAlignedCubesNoSlope' origin innerSDR transposeFactors ))
-- Amount used to transpose a point
type TransposeFactor = Double


