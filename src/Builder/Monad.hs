{-# LANGUAGE TemplateHaskell #-}
{-ToDo:
Get rid of the custom state stuff.

Do a module for extensible-effects
-https://hackage.haskell.org/package/extensible-effects

Do a module for layers
-https://hackage.haskell.org/package/layers
-}
 

module Builder.Monad ( BuilderData(..), combine, combine', combine'',
                      cornerPointsContainError, cornerPointsContainError',
                      eitherCornerPointsContainError',
                      CornerPointsErrorForMonad(..)) where
{- |


Tests are in Tests.BuilderMonadTest
-}
{-
ToDo:

-}

import CornerPoints.CornerPoints(CornerPoints(..))

import Stl.StlBase(Triangle(..))

import Data.List(find)

import Control.Monad (ap)

{------------------------------------------ A monad for building up triangles as the CornerPoints are being made.-------------------------
May end up depracted, if the new StlAutoGenerator works out.
Has been tested in Tests.BuilderMonadTest, but not used anywhere yet.
-}


data TriangleBuilder a =
  TriangleBuilder {_cornerPoints :: a, _triangles :: [Triangle]}
  
instance Functor TriangleBuilder where
  fmap fx (TriangleBuilder cornerpoints' triangles') = TriangleBuilder (fx cornerpoints') triangles'
  
instance Applicative TriangleBuilder where
  pure a = TriangleBuilder a []
  --warning: No explicit implementation for ‘<*>’
  --use as a default to avoid above warning
  (<*>) = ap


instance Monad TriangleBuilder  where
  return a = TriangleBuilder a []
  
  (TriangleBuilder cornerpoints' triangles') >>= fx = let (TriangleBuilder cornerpoints'' triangles'')  = fx cornerpoints'
                                              in
                                                  TriangleBuilder cornerpoints'' (triangles' ++ triangles'' )
  
  
                                                 
  
{- ---------------------------------------------- CornerPointsBuilder-------------------------------------------------

Monad to build up a list of all CornerPoints, as the CornerPoints are being generated.

-}
data BuilderData = CornerPointsData {_cPointsCurr :: [CornerPoints], _cPointsAll :: [CornerPoints]}
                 | CornerPointsErrorData {_err :: String, _cPointsAllPriorToError :: [CornerPoints] }
  deriving (Show, Eq)

{-should this have the cPointsPriorToError?
This seems to be something more related State.
-}
data CornerPointsErrorForMonad = CornerPointsErrorForMonad
                                   { msg :: String
                                     --cPointsPriorToError :: [CornerPoints]
                                   }
     deriving Show

  
--Used for the builder Monad. Should be deleted if State works
cornerPointsContainError :: [CornerPoints] -> Either String [CornerPoints]
cornerPointsContainError cPoints =
  let isError :: CornerPoints -> Bool
      isError (CornerPointsError _) = True
      isError _                     = False

      hasError = find (isError) cPoints
  in  case hasError of
        Nothing  -> Right cPoints
        Just err -> Left $ errMessage err

--this version is for working with the State monad
 
cornerPointsContainError' :: [CornerPoints] -> Bool
cornerPointsContainError' cPoints =
  let isError :: CornerPoints -> Bool
      isError (CornerPointsError _) = True
      isError _                     = False

      hasError = find (isError) cPoints
  in  case hasError of
        Nothing  -> False
        Just err -> True




{-If [CornerPoints] contains an error, return the Left error msg.
If no error, return Right [CornerPoints]-}
eitherCornerPointsContainError' :: [CornerPoints] -> Either CornerPointsErrorForMonad [CornerPoints]
eitherCornerPointsContainError' cPoints =
  let isError :: CornerPoints -> Bool
      isError (CornerPointsError _) = True
      isError _                     = False

      hasError = find (isError) cPoints
  in  case hasError of
        Nothing  -> Right cPoints
        Just err -> Left $ CornerPointsErrorForMonad (errMessage err) 

--Used with the Builder monad. Delete if State works.
combine :: BuilderData -> BuilderData -> BuilderData 
combine ((CornerPointsData curr all)) ((CornerPointsData curr' all')) =
  case (cornerPointsContainError curr') of
     Right _ ->  CornerPointsData (curr') (all' ++ all)
     Left err' -> combine (CornerPointsData curr all) (CornerPointsErrorData err' all')
 
combine ((CornerPointsData curr all)) (CornerPointsErrorData err' cPointsAll') =
  CornerPointsErrorData err' all

{- used for State monad.
If the new cpoints have no error, add them to the cPoints all.
Else return the cPointsAll without the new cPoints.
-}
combine' :: [CornerPoints] -> [CornerPoints] -> [CornerPoints]
combine' cPointsNew cPointsAll =
  if cornerPointsContainError' cPointsNew
     then cPointsAll
     else cPointsNew ++ cPointsAll

{-combine the [CornerPoints] without any checking for error.
That will be left to Except Monad.-}
combine'' :: [CornerPoints] -> [CornerPoints] -> [CornerPoints]
combine'' cPointsNew cPointsAll =
  cPointsNew ++ cPointsAll




