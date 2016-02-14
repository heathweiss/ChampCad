module Stl.StlBase(
Vertex(..),
newVertex,
showVertex,
showTextVertex,
StlShape(..),
Triangle(..),
Loop(..),
newLoop,
FFacetNormals(..),
newFFacetNormals,
openFacetNormals,
closeFacetNormals,
newStlShape,
openStlShape,
closeStlShape,
stlShapeToText
)
where
import CornerPoints.Points (Point(..))
import Text.Printf (printf)

stlShapeToText :: StlShape -> String
stlShapeToText stlFile = openStlShape stlFile
 ++ concat  (map (trianglesToFacetNormalsString (facetNormal stlFile) (loop stlFile) ) (triangles stlFile))
 ++ "\n" ++ closeStlShape stlFile
    
trianglesToFacetNormalsString :: FFacetNormals -> Loop -> Triangle -> String
trianglesToFacetNormalsString facetNormal loop EmptyTriangle =
    "\n" ++ openFacetNormals facetNormal
    ++ "\n" ++ openLoop loop
    ++ "\n" ++ "empty triangle"
    ++ "\n" ++ "empty triangle"
    ++ "\n" ++ "empty triangle"
    ++ "\n" ++ "empty triangle"
    ++ "\n" ++ "empty triangle"




trianglesToFacetNormalsString facetNormal loop triangle =
    "\n" ++ openFacetNormals facetNormal
    ++ "\n" ++ openLoop loop
    ++ "\n" ++ showVertex (v1 triangle )
    ++ "\n" ++ showVertex (v2 triangle )
    ++ "\n" ++ showVertex (v3 triangle )
    ++ "\n" ++ closeLoop loop 
    ++ "\n" ++ closeFacetNormals facetNormal


data StlShape = StlShape {shapeName :: String, solid :: String, endsolid :: String, facetNormal :: FFacetNormals, loop :: Loop, triangles :: [Triangle]}
newStlShape name triangles = StlShape name "solid " "endsolid " newFFacetNormals newLoop triangles

openStlShape :: StlShape -> String
openStlShape (StlShape name solid _ _ _ _) = solid ++ name

closeStlShape :: StlShape -> String
closeStlShape (StlShape _ _ endsolid _ _ _) = endsolid 



data FFacetNormals = FFacetNormals {nnormal1 :: Float, nnormal2 :: Float, nnormal3 :: Float, openFacet :: String, closeFacet :: String}
newFFacetNormals :: FFacetNormals
newFFacetNormals = FFacetNormals 0 0 0 "facet normal " "endfacet" 

openFacetNormals :: FFacetNormals -> String
openFacetNormals (FFacetNormals nn1 nn2 nn3 open close) = (open) ++ show nn1 ++ " " ++ show nn2 ++ " " ++ show nn3

closeFacetNormals :: FFacetNormals -> String
closeFacetNormals facet = closeFacet facet


--data Loop = Loop {triangle :: Triangle, openLoop :: String, closeLoop :: String}
data Loop = Loop {openLoop :: String, closeLoop :: String}
newLoop :: Loop
newLoop = (Loop "outer loop" "endloop" )




data Triangle = Triangle {v1 :: Vertex, v2 :: Vertex, v3 :: Vertex}
 | EmptyTriangle
 deriving (Show) --added this in for testing. Make sure it still works.


data Vertex = Vertex {vertexName :: String, point :: Point}
 deriving (Show) --added this in for testing. Make sure it still works.
newVertex :: Point -> Vertex
newVertex point  = Vertex "vertex " point 

showVertex :: Vertex -> String
showVertex (Vertex vertexName (Point vx vy vz)) = vertexName ++  (printf "%e" (vx)) ++ " " ++ (printf "%e" (negate vy)) ++ " " ++ (printf "%e" (vz))
--added negate on vx while testing radialRiser
showTextVertex :: Vertex -> String
showTextVertex vertex = "\n" ++ (showVertex vertex)





data FacetNormals = FacetNormals {normal1 :: Float, normal2 :: Float, normal3 :: Float}
zeroedFacetNormals :: FacetNormals
zeroedFacetNormals = FacetNormals {normal1=0, normal2=0, normal3=0}

instance Show FacetNormals where
    show (FacetNormals n1 n2 n3) = "facet normal " ++ show n1 ++ " " ++ show n2 ++ " " ++ show n3
