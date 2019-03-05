module Examples.Gmsh.FirstTest() where
{-
Testing of the GMSH modules.
-}

import qualified GMSH.Points as GP
import qualified GMSH.Lines as GL 
import qualified GMSH.Common as GC
import qualified GMSH.Builder as GB
import qualified GMSH.Writer as GW

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints

import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E

{-
Create a FrontFace using the Gmsh Builder.
Used by runGenerateFrontFace.
-}
generateFrontFace :: GB.ExceptStackCornerPointsBuilder
generateFrontFace = do
  frontFace <- GB.buildCubePointsListSingle "FrontFace" [FrontFace (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4)]
  return frontFace

{-
Run generateFrontFace using SL.execState $ E.runExceptT.
Print out the points to a gmsh script.
Does not yet print out lines.
Next: Create code to print out gmsh lines.
-}
runGenerateFrontFace :: IO()
runGenerateFrontFace = do
  let
    builderData = ((SL.execState $ E.runExceptT generateFrontFace ) GB.newBuilderData)
    builderDataWithOpeningValues = "//created by ChampCad Examples.Gmsh.FirstTest" ++
             "\nSetFactory(\"OpenCASCADE\");" ++
             GW.pntsBldrDataScriptFromBlderData builderData

  --output the Points to stdout just to look at it for testing.
  putStrLn builderDataWithOpeningValues
  --write the Points to file, as is required for GMSH
  writeFile "src/Data/gmeshScripts/test.geo" builderDataWithOpeningValues
