module GMSH.Writer.Lines(writeGScriptsToFile) where
import qualified System.IO as SIO
import qualified Data.Text as T

import qualified GMSH.Lines as GL
import qualified GMSH.State as GST
import qualified CornerPoints.Points as Pts
import qualified Helpers.FileWriter as FW

writeGScriptToFile :: SIO.Handle -> GL.Line -> IO ()
writeGScriptToFile h line =
  let
    toGScript :: GL.Line -> T.Text
    toGScript (GL.Line' (GST.LineId' id) (GST.GPointId' idStart) (GST.GPointId' idEnd))  =
      T.pack $
        "\nLine("  ++
          (show (id)) ++ ") = {"  ++
          (show idStart) ++ "," ++
          (show idEnd) ++
          "};"
  
  in
  FW.writeFileUtf8 h $ toGScript line


writeGScriptsToFile :: SIO.Handle -> [GL.Line] -> IO ()
writeGScriptsToFile h lines =
  mapM_ (writeGScriptToFile h) lines
