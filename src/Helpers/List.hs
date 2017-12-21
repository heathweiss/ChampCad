module Helpers.List((++:), (++::), SplitAtT(..), splitAndReverseBackHalf, removeEmpty, safeHead, safeTail) where

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


-- | Clear the [[CornerPoints]] of inner perimeters of any []
removeEmpty :: [[a]] -> [[a]]
removeEmpty inputList =
  let clearFx ::  [innerPerimeters] -> Bool
      clearFx inputList = (length inputList) > 0

  in
    filter (clearFx) inputList

safeHead :: [[a]] -> [a]
safeHead (x:xs) = x
safeHead [] = []

safeTail :: [[a]] -> [[a]]
safeTail (x:xs) = xs
safeTail [] = []
