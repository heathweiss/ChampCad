
module Examples.Primitives.Cube(writeJoinerStlFile) where
import Stl.StlCornerPoints((|+++^|))
import Stl.StlCornerPoints( Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import Primitives.Cubical( rectangularSolidNoSlope)
import CornerPoints.Radius(Radius(..))
import CornerPoints.Points(Point(..))
import CornerPoints.Create( Slope(..), Origin(..), createCornerPoint, createCornerPointSquaredOff, Angle(..))

writeJoinerStlFile = writeStlToFile stlFile
stlFile = newStlShape "joiner cube" rectangleTriangles

rectangle = rectangularSolidNoSlope (map (Radius) [10,10..])  (Point 0 0 0) (map (Angle) [20,160,200,340,20]) 2 
rectangleTriangles = [FacesBottomFrontTop | x <- [1..]] |+++^| rectangle

