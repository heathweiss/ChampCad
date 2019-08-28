module GMSH.Base(NonOverLappedClosed(..)) where

{- |
A [] in which the contents have no overlap in that no 2 consecutive items are ==. This represents the NonOverLapped state.
So to make [GMESH.Line], will need to reuse each point(except head and last) for making 2 lines.
The last Point will be the same as head, which gives the closed state.

This state cannot be enforced, as the constructor is exported. It is up to the user to treat it like a law.
Always ensure the contained list is in the required state.

Known uses:
[CornerPoints.Points] must be in this state to make a [CurvePoints].
The resulting [CurvePoints] will also be returned in this state, which is required to make a [Curves].
-}
newtype NonOverLappedClosed a  = NonOverLappedClosed a
