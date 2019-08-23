module GMSH.Writer.GPoints(writeGScriptToFile) where
import qualified System.IO as SIO
import qualified Data.Text as T

import qualified CornerPoints.Points as Pts
import qualified GMSH.State as GST
import qualified Helpers.FileWriter as FW
{-
https://stackoverflow.com/questions/26778415/using-overloaded-strings
-}


writeGScriptToFile :: SIO.Handle -> GST.GPointId -> Pts.Point -> IO ()
writeGScriptToFile h gPointId point =
  let
    toGScript :: GST.GPointId -> Pts.Point -> T.Text
    toGScript (GST.GPointId' id) (Pts.Point x y z) =
      T.pack $
        "\nPoint(" ++
          (show (id)) ++ ") = {"  ++
          (show x) ++ "," ++
          (show y) ++ "," ++
          (show z) ++ "};"

  in
  FW.writeFileUtf8 h $ toGScript gPointId point


