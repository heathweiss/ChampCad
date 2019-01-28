module GMSH.Common(Changes(..)) where


{- |
Wrap 'a' to show if it has been changed form its original state.
toDo:
Extract this to a higher level module, as it probably will be used by 'Lines' and other Gmsh modules.

Known uses:
'insert' may or may not insert a hashed point and gmsh id, into a Data.HashMap.Strict, depending on if it already existed.
This will have to be know to maintain the state in the GMSH.Builder monad, in regards to generating GMSH id's.

-}
data Changes a =
  Changed a
  |
  UnChanged a
  deriving (Eq, Show)
