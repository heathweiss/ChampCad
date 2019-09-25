{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{- |
Convert ChampCad Points/Lines/etc to gmsh scripts and print to .geo file.
-}

module GMSH.Writer(openFile, writeComment, writeSeparator0, writeSeparator1, writeSeparator2, writeSeparator3, writeSeparator4,
                   Scriptable, writeScript, writeScripts, showId) where

import CornerPoints.Points(Point(..))
--import qualified GMSH.Common as GC
--import qualified GMSH.Points as GP

import qualified System.IO as SIO
import qualified Data.Text as T
import qualified Helpers.FileWriter as FW

default (T.Text)


{- |
Given
fileName: Name of the file to write.

Task
Open a handle to a file in "src/Data/gmshScripts/fileName"

Return
A handle in WriteMode.

Known uses
Write GMSH script files when using GMSH Builder monad stack.
-}
openFile :: String -> IO (SIO.Handle)
openFile fileName = SIO.openFile  ("src/Data/gmshScripts/" ++ fileName ++ ".geo") SIO.WriteMode
  

{- |
Given
h: Handle to file.
comment: The comment to write to gmsh script file.

Task
Write a gmsh style comment to the file.
Prepend it with a newline.

Return
Side effect of comment written to the file.
-}
writeComment :: SIO.Handle -> String -> IO ()
writeComment h comment =
  FW.writeFileUtf8 h $ T.pack $ "\n//" ++ comment



{- |
Given
h: Handle to file.

Task
Write a gmsh style comment to the file that exists of a line of //'s.
Prepend it with 5 newlines.

Return
Side effect of comment written to the file.

Know uses
Create a separator in a gmsh script file to break the file up into logical sections.
Various versions will create various white space before as an effect of separation.
-}
writeSeparator0 :: SIO.Handle -> IO ()
writeSeparator0 h =
  FW.writeFileUtf8 h $ T.pack $ "/////////////////////////////////////////////////////////////////////////////"

writeSeparator1 :: SIO.Handle -> IO ()
writeSeparator1 h =
  FW.writeFileUtf8 h $ T.pack $ "\n/////////////////////////////////////////////////////////////////////////////"

writeSeparator2 :: SIO.Handle -> IO ()
writeSeparator2 h =
  FW.writeFileUtf8 h $ T.pack $ "\n\n/////////////////////////////////////////////////////////////////////////////"

writeSeparator3 :: SIO.Handle -> IO ()
writeSeparator3 h =
  FW.writeFileUtf8 h $ T.pack $ "\n\n\n/////////////////////////////////////////////////////////////////////////////"

writeSeparator4 :: SIO.Handle -> IO ()
writeSeparator4 h =
  FW.writeFileUtf8 h $ T.pack $ "\n\n\n\n/////////////////////////////////////////////////////////////////////////////"

{- |
For writing GMSH ADT's to a file as gmsh script.
Known Instances: CurvePoints, Curves

-}
class Scriptable a where
  writeScript :: SIO.Handle -> a -> IO ()
  writeScripts :: SIO.Handle -> [a] -> IO ()
  writeScripts h as = mapM_ (writeScript h) as
  showId :: a -> String
  -- | Show the Id of a GMSH types including Curve, CurvePoint...
