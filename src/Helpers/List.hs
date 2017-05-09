module Helpers.List((++:), (++::), SplitAtT(..), splitAndReverseBackHalf ) where

type SplitAtT = Int

infixl 3 ++:

-- |Appends a list onto a list of lists.
-- Known uses: building up a list of Faces to be zipped to list of Cornerpoints using (+++^) for createing stl output
--give it a lower infix than ++
(++:) :: [[a]] -> [a] -> [[a]]
multiList ++: singleList = reverse $ singleList : (reverse multiList)



{- |

-}
(++::) :: [[a]] -> [[a]] -> [[a]]
multiList1 ++:: multiList2 = multiList1 ++ multiList2

infixl 2 ++::


splitAndReverseBackHalf :: SplitAtT -> [a] -> ([a],[a])
splitAndReverseBackHalf splitListAt measurements =
  let (front,back) = splitAt splitListAt measurements
  in
  (front, reverse back)
