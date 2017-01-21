module CornerPoints.Degree (Degree(..)) where

--ToDo: Look at remving this module. CornerPoints.CornerPointsWithDegrees is only module that seems to use it,
--and that is another module that is perhaps obsolete now that stl can be auto-generated.

{- |Anywhere that a degree of a circle is required, use this to make type signatures more readable.
Having it here in a single place, saves declaring it in multiple files where more readable type signatures are required..-}
type Degree = Double

