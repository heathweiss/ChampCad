{-# LANGUAGE OverloadedStrings #-}

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
import qualified System.IO as SIO
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as DB


{-
https://www.snoyman.com/blog/2016/12/beware-of-readfile
Snoyman says safest and ~fastest way to write strings to file.
Make a version for Text and String.
Known uses:
Write the gmsh script to file in generateFrontFace.
-}
writeFileUtf8 :: SIO.Handle -> T.Text -> IO ()
writeFileUtf8 handle t = DB.hPutStr handle $ TE.encodeUtf8 t

writeFileUtf8_str :: SIO.Handle -> String -> IO ()
writeFileUtf8_str handle t = DB.hPutStr handle $ TE.encodeUtf8 $ T.pack t

{-
Create a FrontFace using the Gmsh Builder.
Used by runGenerateFrontFace.
-}
-- rewrite with IO at bottom of GB.ExceptStackCornerPointsBuilder
--write the frontFace to a file using: replace SIO with https://www.snoyman.com/blog/2016/12/beware-of-readfile
generateFrontFace :: GB.ExceptStackCornerPointsBuilder
generateFrontFace = do
  h <- E.liftIO $ SIO.openFile  "src/Data/test.txt" SIO.WriteMode
  frontFace <- GB.buildCubePointsListSingle "FrontFace"
                 [FrontFace (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4),
                  FrontFace (Point 11 11 11) (Point 12 12 12) (Point 13 13 13) (Point 14 14 14)
                 ]
  
  E.liftIO $ writeFileUtf8_str h $ show frontFace
  
  E.liftIO $  SIO.hClose h
  return frontFace



{-
Run generateFrontFace using SL.execState $ E.runExceptT and an empty GB.BuilderData
-}

runGenerateFrontFace :: IO()
runGenerateFrontFace = do
  --((SL.execStateT $ E.runExceptT generateFrontFace ) GB.newBuilderData)
  io <- ((SL.execStateT $ E.runExceptT generateFrontFace ) GB.newBuilderData)

  --Look at the 'return frontFace'
  --print $ show $ io
  
  putStrLn "done"


