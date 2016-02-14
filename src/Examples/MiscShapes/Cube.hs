module Examples.Shapes.Cube(hello, writeJoinerStlFile, joinerCube) where
import Primitives.Cubical(rectangularCube)
import TriCad.StlCornerPoints((+++^))
import TriCad.CornerPoints( Faces(..))
import TriCad.StlBase (StlShape(..), newStlShape)
import TriCad.StlFileWriter(writeStlToFile)

writeJoinerStlFile = writeStlToFile stlFile
stlFile = newStlShape "joiner cube" joinerTriangles

joinerTriangles =
  [FacesAll]
  +++^
  joinerCube
  
joinerCube =
  [rectangularCube 2 25 50]

hello = do
  putStrLn "hello from cube"
