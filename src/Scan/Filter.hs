module Scan.Filter(runningAverage, averageValueOf, runningAvgSingleDegreeRadii) where
import CornerPoints.Radius(SingleDegreeRadii(..),Radius(..))
import qualified Data.List as L
--import CornerPoints.VerticalFaces(SingleDegreeRadii(..))

type NumberOfRadius = Double
{-
Take a number to indicate the # of items to start with.
-}

runningAvgSingleDegreeRadii :: NumberOfRadius ->  SingleDegreeRadii -> SingleDegreeRadii
runningAvgSingleDegreeRadii    lengthOfRunningAvgList   (SingleDegreeRadii degree' radii') =
  SingleDegreeRadii degree' (runningAverage lengthOfRunningAvgList radii')

runningAverage :: NumberOfRadius -> [Radius] -> [Radius]
runningAverage    lengthOfRunningAvgList    (x:xs)     =

  let runningAvgList  = [x | z <- [1..lengthOfRunningAvgList]]
      
  in  
     averageValueOf runningAvgList : (runningAverageRecur (init runningAvgList) xs)

{-
runningAverage    numberOfRadius    radii     =

  let startingRadius  = [head radii | x <- [1..numberOfRadius]]
      
  in  
     averageValueOf startingRadius : (runningAverageRecur (tail startingRadius) $ tail radii)
-}


runningAverageRecur :: [Radius] ->     [Radius] ->[Radius]
runningAverageRecur    runningAvgList  (x:xs) =
  let runningAvgList' =  x : runningAvgList
      avg = averageValueOf runningAvgList'
      
  in  avg : runningAverageRecur (init runningAvgList') xs

runningAverageRecur    runningAvgList  [] = []

averageValueOf :: [Radius] -> Radius
averageValueOf list = Radius $ (L.sum $ map (radius) list)  / (fromIntegral $ length list)
