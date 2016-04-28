{-# LANGUAGE TemplateHaskell #-}

module Builder.Builder(CornerPointsBuilder(..),(&+++#@),{- (@~+++^),-} {-(|@~?+++^|),-} {-FacesWithRange(..), (||@~?+++^||), processCornerPointsWithDegreesAndStl-}
                       {-, (&@~+++@),(&@~+++#@), (||@~+++^||),
                       , newCornerPointsWith10DegreesBuilder-}) where
import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.CornerPointsWithDegrees(DegreeRange(..), CornerPointsWithDegrees(..), cornerPointsWithDegreesWithinRange, (@~+++#@), (|@~+++#@|), (|@~+++@|),
                                           newCornerPointsWith10DegreesList)
import Stl.StlBase(Triangle(..))
import Stl.StlCornerPoints((+++^), Faces(..))
import CornerPoints.Degree(Degree(..))

-- for Builder, which may be removed.
import Control.Lens
import Test.HUnit hiding (State)

-- ======================================= CornerPointsBuilder ===============================
{-Should be able to delete later on, to be replaced by CornerPointsWithDegrees.
Will need to change walkerSocket(Squared) first.

Should have just used a [[CornerPoints]] instead of new data type to hold them.
(&+++#@) could then have been #@:||@|| which is to say:
  Function mapped on the head [Cornerpoint], and appended to the [[CornerPoints]]-}

--ToDo: Make an instance of Monad, and try building shapes with Monad.
-- |=================== Depracated. Use CornerPointsWithDegrees in this module. ==============
--Building up a shape usually involves [[CornerPoints]]. This allow use of infix operators
--  to build up the shape in an monadic way, along with the use of &+++#@.
data CornerPointsBuilder   = CornerPointsBuilder {getCornerPoints :: [[CornerPoints]]}
  deriving (Eq, Show)

           
-- |=================== Depracated. Use CornerPointsWithDegrees in this module. ==============
--The infix operator to go along with CornerPointsBuilder for building up shapes as [[CornerPoints]]
(&+++#@) :: CornerPointsBuilder -> ([CornerPoints] -> [CornerPoints]) -> CornerPointsBuilder
(CornerPointsBuilder cornerPoints) &+++#@ f = CornerPointsBuilder ( (f $ head cornerPoints) : cornerPoints)


-----------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
--Have not tried this out yet. First see if I can get a mondad builder working.

data Builder = Builder {_cornerPoints :: [CornerPoints], _triangles :: [Triangle] }

makeLenses ''Builder

(++&) :: Builder -> ([CornerPoints] -> Builder ) -> Builder
(Builder c t) ++& f =
  let Builder c' t'  = f c
      
  in  Builder c' (t ++ t')  

runBuilder :: Builder -> [Triangle]
runBuilder builder = builder^.triangles


builderTest = do
  let testing = TestCase $ assertEqual
        "testing"
        False
        True
  runTestTT testing
