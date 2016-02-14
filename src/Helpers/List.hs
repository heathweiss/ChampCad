module Helpers.List((++:), (++::)) where
-- |Appends a list onto a list of lists.
-- Known uses: building up a list of Faces to be zipped to list of Cornerpoints using (+++^) for createing stl output
--give it a lower infix than ++
infixl 3 ++:


(++:) :: [[a]] -> [a] -> [[a]]
multiList ++: singleList = reverse $ singleList : (reverse multiList)



{- |

-}
(++::) :: [[a]] -> [[a]] -> [[a]]
multiList1 ++:: multiList2 = multiList1 ++ multiList2

infixl 2 ++::

