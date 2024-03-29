{-# LANGUAGE DeriveDataTypeable #-}
-- {-# ConstrainedClassMethods #-}
module TypeClasses.Showable(showConstructor, Showable()) where

import  Data.Data
import  Data.Typeable

{- |
Use Data.<Data/Typeable> to find information about an ADT and it's constructors.

-}
class (Typeable a, Data a) => Showable a where
  showConstructor ::  a -> String
  showConstructor a = showConstr . toConstr $ a
  -- | Show the constructor of a ADT.
  -- | Known uses. Have a default pattern match that will supply an error string instead of a failing pattern match.
  -- | It will be able to show which constructor it was that did not have a pattern match.
  --showId :: a -> String
  -- -- | Show the Id of a GMSH type.

{-
class Showable a where
  showConstructor :: (Typeable a, Data a) => a -> String
  showConstructor a = showConstr . toConstr $ a
  -- | Show the constructor of a ADT.
  -- | Known uses. Have a default pattern match that will supply an error string instead of a failing pattern match.
  -- | It will be able to show which constructor it was that did not have a pattern match.
  --showId :: a -> String
  -- -- | Show the Id of a GMSH type.

-}
