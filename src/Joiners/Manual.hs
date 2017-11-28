{-|
Use for joining together 2 [CornerPoints] which have to be matched up.
example: Cutting a cylinder out of a radial shape such as the pillar cylinders in a shoe scan.

Does it in a very manual system by:
From each of the [CornerPoints]:

Extract the face, or build a face from 1 line of the face.
Can also create a face from the previous face, as is required when the [CornerPoints] are not the same length,
most likely because they have different Angles.
Eg: Join a cylinder with angles of every 10 degrees to a scan taken every 5 degrees.

When all done Joining, there will be 2 new [CornerPoints] which:
-should be the same length. Of course 1 may be shorter which would be the liminting list.
-can now be |+++| together.
-}

module Joiners.Manual(Joiner(..),joiner, takeLeading, takeTrailing) where

import CornerPoints.CornerPoints(CornerPoints(..), )
import CornerPoints.FaceConversions(toBackFace, toFrontFace)
import CornerPoints.FaceExtraction(extractFrontLeftLine, extractFrontRightLine, extractBackRightLine, extractBackLeftLine)
 
type OriginalCpoints = [CornerPoints] 
type JoinedCpoints = [CornerPoints]

{-
When removing the collar, or other joinging of 2 mismatched [CornerPoints]:
Decide if Take the next Cpoint, use the same one again, or some part of them such as:
extract FrontLeftLine and make it into a front face

This data type will be used for pattern matching to make those decisions.
-}
data Joiner =
         Take
       | TakeLeading
       | TakeTrailing
       | Hold
       | HoldTrailing
       | HoldLeading
       
{-
modify a [Cpoints] by zipping with a [Joiner]

given:
takeLeadingFx: Take the <Front/Back>Face, extract the Left line and turn into a <Back/Front> Face.
takeTrailingFx: Take the <Front/Back>Face, extract the Right line and turn into a <Back/Front> Face.
-}
joiner :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints) -> [Joiner] -> [CornerPoints] ->  [CornerPoints]
joiner _ _ [] _ =
  [CornerPointsError "empty [Joiner] passed into joiner"]
joiner takeLeading takeTrailing joiners [] =
  [CornerPointsError "empty [CornerPoints] passed into joiner"]

joiner takeLeading takeTrailing (join:joins) cpoints  =
  --append head on to give an initial prev cpoint.
  joiner' takeLeading takeTrailing join joins CornerPointsNothing cpoints []


  

{-
inner recursive call for joiner.
terms:
 <>UnjoinedCpoint: the original [CornerPoints] which have yet to be zipped with the [Joiner]
 joinedCpoints: The [CornerPoints] that are the result of zipping the original [CornerPoints] with the [Joiner]
-}

joiner' :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints) -> Joiner -> [Joiner] -> CornerPoints -> OriginalCpoints -> JoinedCpoints -> JoinedCpoints
{-
joiner'   _ _ _ _ _ [] joinedCpoints  =
  reverse joinedCpoints
-}
joiner'   takeLeading takeTrailing TakeTrailing (join:joins) prevUnjoinedCpoint (currUnjoinedCpoint : origCpoints) joinedCpoints  =
  joiner' takeLeading takeTrailing join joins currUnjoinedCpoint (origCpoints) ((takeTrailing currUnjoinedCpoint):joinedCpoints  )

joiner'   takeLeading takeTrailing TakeLeading (join:joins) prevUnjoinedCpoint (currUnjoinedCpoint : origCpoints) joinedCpoints  =
  joiner' takeLeading takeTrailing join joins currUnjoinedCpoint (origCpoints) ((takeLeading currUnjoinedCpoint):joinedCpoints  )



joiner' takeLeading takeTrailing HoldLeading (join:joins) prevUnjoinedCpoint (currUnjoinedCpoint : origCpoints) joinedCpoints =
  joiner' takeLeading takeTrailing join joins prevUnjoinedCpoint (currUnjoinedCpoint : origCpoints) ((takeLeading prevUnjoinedCpoint):joinedCpoints  )

joiner' takeLeading _ HoldLeading [] prevUnjoinedCpoint _ joinedCpoints =
  reverse ((takeLeading prevUnjoinedCpoint):joinedCpoints  )

joiner' takeLeading _ HoldLeading _ prevUnjoinedCpoint [] joinedCpoints =
  reverse ((takeLeading prevUnjoinedCpoint):joinedCpoints  )



joiner' _ takeTrailing HoldTrailing [] prevUnjoinedCpoint _ joinedCpoints =
  reverse ((takeTrailing prevUnjoinedCpoint):joinedCpoints  )

joiner' takeLeading takeTrailing HoldTrailing (join:joins) prevUnjoinedCpoint  (currUnjoinedCpoint : origCpoints) joinedCpoints =
  joiner' takeLeading takeTrailing join joins prevUnjoinedCpoint (currUnjoinedCpoint : origCpoints) ((takeTrailing prevUnjoinedCpoint):joinedCpoints  )

joiner'   _ takeTrailing TakeTrailing [] _ (currUnjoinedCpoint : origCpoints) joinedCpoints  =
  reverse  $ (takeTrailing currUnjoinedCpoint):joinedCpoints

joiner'   takeLeading _ TakeLeading [] _ (currUnjoinedCpoint : origCpoints) joinedCpoints  =
  reverse  $ (takeLeading currUnjoinedCpoint):joinedCpoints



joiner' _ _ Take [] _ (currUnjoinedCpoint : origCpoints) joinedCpoints  =
  reverse $ currUnjoinedCpoint : joinedCpoints

joiner' takeLeading takeTrailing Take (nextJoin:[]) _ ( currUnjoinedCpoint : origCpoints) joinedCpoints  =
  joiner' takeLeading takeTrailing nextJoin [] currUnjoinedCpoint  (origCpoints) (currUnjoinedCpoint:joinedCpoints)

joiner' takeLeading takeTrailing Take (nextJoin:joins) _ (currUnjoinedCpoint : origCpoints) joinedCpoints  =
  joiner' takeLeading takeTrailing nextJoin joins currUnjoinedCpoint (origCpoints) (currUnjoinedCpoint:joinedCpoints)

joiner' takeLeading takeTrailing Take _ _ [] joinedCpoints  =
  reverse joinedCpoints

joiner' takeLeading takeTrailing Hold (nextJoin:joins) prevUnjoinedCpoint (currUnjoinedCpoint : origCpoints) joinedCpoints  =
  joiner' takeLeading takeTrailing nextJoin joins currUnjoinedCpoint (origCpoints) (prevUnjoinedCpoint:joinedCpoints)


--extract <back/front>LeftLine and turn it into a <back/front>Face
takeLeading :: CornerPoints -> CornerPoints
takeLeading (FrontFace p1 p2 p3 p4) = (toFrontFace . extractFrontLeftLine) (FrontFace p1 p2 p3 p4)
takeLeading (BackFace p1 p2 p3 p4)  = (toBackFace . extractBackLeftLine) (BackFace p1 p2 p3 p4)
--extract <back/front>RightLine and turn it into a <back/front>Face
takeTrailing :: CornerPoints -> CornerPoints
takeTrailing (FrontFace p1 p2 p3 p4) = (toFrontFace . extractFrontRightLine) (FrontFace p1 p2 p3 p4)
takeTrailing (BackFace p1 p2 p3 p4)  = (toBackFace . extractBackRightLine) (BackFace p1 p2 p3 p4)
