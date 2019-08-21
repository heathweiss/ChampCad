module GMSH.GPoints() where
{- |
Nothing is here so far, as all GPointId work is done in GMSH.State.
Should be able to get rid of this module.
-}

--import qualified GMSH.State as GST
--import qualified GMSH.Point as GPts

-- | A [GMSH.State.GPointId] in which the GPpointIds have no overlap, so to make [GMESH.Line], will need to reuse each point(except head and last) for making 2 lines.
-- | The last Point will be the same as head.
-- | Does not have the constructor exported, as the the fx: toNonOverlappingClosedGPoint is the only way to get to this state.
--newtype NonOverLappedClosedGPoint = NonOverLappedClosedGPoints GST.GPointId
--wait till the version for Points is changed, as it contains a list. That is a mistake.
--toNonOverlappingClosedGPoint :: GPts.NonOverLappedClosedPoints -> NonOverLappedClosedGPoints


