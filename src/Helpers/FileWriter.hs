module Helpers.FileWriter(writeFileUtf8, writeFileUtf8_str) where

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
