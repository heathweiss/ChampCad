{-|
Use for joining together 2 [CornerPoints] which have to be matched up.
example: Cutting a cylinder out of a radial shape such as the pillar cylinders in a shoe scan.
-}

module Builder.Joiner(Joiner(..),joiner, takeLeft, takeRight) where

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
       | TakeLeft
       | TakeRight
       | Hold
       | HoldRight
       | HoldLeft
       
{-
modify a [Cpoints] by zipping with a [Joiner]

given:
takeLeftFx: Take the <Front/Back>Face, extract the Left line and turn into a <Back/Front> Face.
takeRightFx: Take the <Front/Back>Face, extract the Right line and turn into a <Back/Front> Face.
-}
joiner :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints) -> [Joiner] -> [CornerPoints] ->  [CornerPoints]
joiner _ _ [] _ =
  [CornerPointsError "empty [Joiner] passed into joiner"]
joiner takeLeft takeRight joiners [] =
  [CornerPointsError "empty [CornerPoints] passed into joiner"]

joiner takeLeft takeRight (join:joins) cpoints  =
  --append head on to give an initial prev cpoint.
  joiner' takeLeft takeRight join joins CornerPointsNothing cpoints []


  

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
joiner'   takeLeft takeRight TakeRight (join:joins) prevUnjoinedCpoint (currUnjoinedCpoint : origCpoints) joinedCpoints  =
  joiner' takeLeft takeRight join joins currUnjoinedCpoint (origCpoints) ((takeRight currUnjoinedCpoint):joinedCpoints  )

joiner'   takeLeft takeRight TakeLeft (join:joins) prevUnjoinedCpoint (currUnjoinedCpoint : origCpoints) joinedCpoints  =
  joiner' takeLeft takeRight join joins currUnjoinedCpoint (origCpoints) ((takeLeft currUnjoinedCpoint):joinedCpoints  )



joiner' takeLeft takeRight HoldLeft (join:joins) prevUnjoinedCpoint (currUnjoinedCpoint : origCpoints) joinedCpoints =
  joiner' takeLeft takeRight join joins prevUnjoinedCpoint (currUnjoinedCpoint : origCpoints) ((takeLeft prevUnjoinedCpoint):joinedCpoints  )

joiner' takeLeft _ HoldLeft [] prevUnjoinedCpoint _ joinedCpoints =
  reverse ((takeLeft prevUnjoinedCpoint):joinedCpoints  )

joiner' takeLeft _ HoldLeft _ prevUnjoinedCpoint [] joinedCpoints =
  reverse ((takeLeft prevUnjoinedCpoint):joinedCpoints  )



joiner' _ takeRight HoldRight [] prevUnjoinedCpoint _ joinedCpoints =
  reverse ((takeRight prevUnjoinedCpoint):joinedCpoints  )

joiner' takeLeft takeRight HoldRight (join:joins) prevUnjoinedCpoint  (currUnjoinedCpoint : origCpoints) joinedCpoints =
  joiner' takeLeft takeRight join joins prevUnjoinedCpoint (currUnjoinedCpoint : origCpoints) ((takeRight prevUnjoinedCpoint):joinedCpoints  )

joiner'   _ takeRight TakeRight [] _ (currUnjoinedCpoint : origCpoints) joinedCpoints  =
  reverse  $ (takeRight currUnjoinedCpoint):joinedCpoints

joiner'   takeLeft _ TakeLeft [] _ (currUnjoinedCpoint : origCpoints) joinedCpoints  =
  reverse  $ (takeLeft currUnjoinedCpoint):joinedCpoints



joiner' _ _ Take [] _ (currUnjoinedCpoint : origCpoints) joinedCpoints  =
  reverse $ currUnjoinedCpoint : joinedCpoints

joiner' takeLeft takeRight Take (nextJoin:[]) _ ( currUnjoinedCpoint : origCpoints) joinedCpoints  =
  joiner' takeLeft takeRight nextJoin [] currUnjoinedCpoint  (origCpoints) (currUnjoinedCpoint:joinedCpoints)

joiner' takeLeft takeRight Take (nextJoin:joins) _ (currUnjoinedCpoint : origCpoints) joinedCpoints  =
  joiner' takeLeft takeRight nextJoin joins currUnjoinedCpoint (origCpoints) (currUnjoinedCpoint:joinedCpoints)

joiner' takeLeft takeRight Take _ _ [] joinedCpoints  =
  reverse joinedCpoints


--extract <back/front>LeftLine and turn it into a <back/front>Face
takeLeft :: CornerPoints -> CornerPoints
takeLeft (FrontFace p1 p2 p3 p4) = (toFrontFace . extractFrontLeftLine) (FrontFace p1 p2 p3 p4)
takeLeft (BackFace p1 p2 p3 p4)  = (toBackFace . extractBackLeftLine) (BackFace p1 p2 p3 p4)
--extract <back/front>RightLine and turn it into a <back/front>Face
takeRight :: CornerPoints -> CornerPoints
takeRight (FrontFace p1 p2 p3 p4) = (toFrontFace . extractFrontRightLine) (FrontFace p1 p2 p3 p4)
takeRight (BackFace p1 p2 p3 p4)  = (toBackFace . extractBackRightLine) (BackFace p1 p2 p3 p4)
