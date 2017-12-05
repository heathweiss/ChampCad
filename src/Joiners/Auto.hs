module Joiners.Auto() where

{- |
Joins together 2 [AngleHeightRadius] by automatically joining them together by:
terms:
outerAHR: outerAHR.
innerAHR: innerAHR.
Note that this is a level below the usual [CornerPoints].
The resulting 2 [AHR] will be used to generate the 2 [CornerPoints].
-These will be the same length.

Have them both start at about the same angle.
-Later, make it so it will adjust itself to a starting position.


-Start with outerAHR as leadingAHR and innerAHR as trailingAHR.
 -If currtrailingAHR.angle < currOuterAHR.angle
   -Append curr of each to corresponding list.
   -call recur with tail of trailingAHR and full outerAHR
 -if trailingAHR.angle ==  currOuterAHR.angle
  -appenAppend curr of each to corresponding list.
  -call recur with tail of each 
 -if trailingAHR.angle >  currOuterAHR.angle
  -call recur so their position as <leading/trailing>AHR is reversed.
 -if (leading:[]) (trailing:[])
  -reverse the lists and exit
 -if (leading:[]) _
  -add prev leading, and currTrailing to lists
  -call recur with [] and tail trailing
 -if _ (trailing:[])
  -call recur with tail leading and []

Should not exit till both used up.

Limitations:
Does not intialize itself if starting angles are not aligned.

Will not choose the best angle, as it does not choose which AHR to connect to
beyond being the head of opposite list.
-}
