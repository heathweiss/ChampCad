{--------------------------------- Purpose------------------------------------
Mirror the use of TriCad.CornerPoints +++ ++++ +++^ functionality, but with error results.

When an illegal +++ ++++ operation is performed, such as RightFace +++ RightFace, a CornerPointsError structure
is returned. This module is for collecting/viewing the errors that are generated.

See Tests.CornerPointsDebugTest for testing/examples of use.

-}
module CornerPoints.Debug((+++^?), (++^?), CubeName(..), CubeDebug(..), CubeDebugs(..), showDebugMsg) where
import CornerPoints.CornerPoints (CornerPoints(..))
import Control.Applicative

--the name of the cube or list of cubes. Gets attached to the debug structure, to make is more informative.
data CubeName = CubeName {name :: String}
--Attaches a CubeName to a CornerPointsError.
--A list of these is what is generated and displayed.
data CubeDebug = CubeDebug {errMsg :: String} deriving (Show)
--known uses: testing
instance Eq CubeDebug where
    CubeDebug msg1 == CubeDebug msg2  
       | msg1 == msg2 = True 
       | otherwise = False 


--known use: allows use of CubeDebug list in QausiQuotations of the html output forall loop as used by Yesod. 
--It should not need it, but that is what makes it work.
--It would be nice to get rid of it. Yesod is going to get rid of QuasiQuotations for output, perhaps then.
data CubeDebugs = CubeDebugs {errMsgs :: [CubeDebug]}

--attach a name to a CornerPointsError, so that is is more usefull for debugging
(++^?) :: CubeName -> CornerPoints -> CubeDebug
(CubeName name) ++^? (CornerPointsError msg) = CubeDebug  (name ++ ": " ++ msg)
(CubeName name) ++^? _ = CubeDebug  (name ++ ": good")

(+++^?) :: [CubeName] -> [CornerPoints] -> [CubeDebug]
infix 3 +++^?
names +++^? cornerPoints =  zipWith (++^?) names cornerPoints 

showDebugMsg :: CubeDebug -> String
showDebugMsg (CubeDebug msg) = msg
