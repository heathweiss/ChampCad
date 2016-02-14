module Stl.StlFileWriter(writeStlToFile, writeStlDebugToFile)  
where
import Stl.StlBase(stlShapeToText, StlShape(..))
import CornerPoints.Debug((+++^?), (++^?), CubeName(..), CubeDebug(..), CubeDebugs(..), showDebugMsg)
import System.IO

{-
Writes the temp.stl file to /src/Data/temp.stl folder when run from emacs repl.
Should have a config file in which this can be set.
given:
  shape: an StlShape generated form a file such as HeelGenerators, or Sockets
return:
  null as it is an IO() action.
  temp.stl file with the stlShape written to it. Will overrite whatever was in the file.
  Fails if file does not already exist.

Known uses:
All code which outputs the final stl, uses this to write the stl file.
-}
writeStlToFile :: StlShape -> IO()
writeStlToFile shape =  writeFile "src/Data/temp.stl" $ stlShapeToText shape

{-
Writes the debug.txt file to src/Data/debug.txt folder when run from emacs repl.
Should have a config file in which this can be set.
given:
  shape: an [CubeDebug] generated from a file such as HeelGenerators, or Sockets
return:
  null as it is an IO() action.
  debug.txt file with the CubeDebug written to it. Will overrite whatever was in the file.
  Fails if file does not already exist.

Needs to be formatted with /n. Should it be done here or in the CornerPointsDebug module?

Known uses:
BlackRunnerHeel uses this to write of the stl file.
-}
writeStlDebugToFile :: [CubeDebug] -> IO()
writeStlDebugToFile debugs = writeFile "src/Data/debug.txt" $ show debugs
