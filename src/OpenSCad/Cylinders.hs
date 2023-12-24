module OpenSCad.Cylinders() where

import OpenSCad.Dimensions(ZHeight(..), newZHeight)
import OpenSCad.Exceptions(ScriptingError(..)) 

import CornerPoints.Radius(Radius(..))

import RIO
import qualified RIO.Text as T
import qualified Prelude as P
import Test.HUnit

data Cylinder = Cylinder {cylHeight :: ZHeight, cylRadius :: Radius, cylSides :: Sides}
 deriving (Eq,Show)

-- | Represents a OpenSCad cylinder. Have not yet created a script instance.
-- Once I do, I can test it out with the geoflex shoe, in which I used a 'OpenSCad.ScriptBase.Script.RawUtf8' cylinder.
data Sides = Sides {sides :: Int}
 deriving (Eq,Show)

newSides :: Int -> Either ScriptingError Sides
newSides int = if int < 3
    then  Left LessThan3Sides
    else  Right $ Sides int

newRadius :: Double -> Either ScriptingError Radius
newRadius radius = 
    if radius <= 0 
     then Left LessThanOrEqTo0Radius
     else Right $ Radius radius

--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------- local testing -------------------------------------------------------------------------
dimensionTests :: IO ()
dimensionTests = do
 P.putStrLn "================================= OpenSCad.Dimensions ===================================================="
 let 
  lookAtInvalidXLenthError = TestCase 
   (do
     assertEqual "invalid XLength show an error msg as a script output"
      (Left LessThanOrEqTo0Radius) 
      (Cylinder <$> (newZHeight 10) <*> (newRadius 0.0) <*> (newSides 4) )
   )
 _ <- runTestTT lookAtInvalidXLenthError

 return ()