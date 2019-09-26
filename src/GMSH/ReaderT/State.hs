{- |
Provides the environment for the ReaderT pattern.
https://tech.fpcomplete.com/haskell/library/rio

Also google: "fpcomplete readerT pattern" for some other takes on this pattern such as:
-https://www.tweag.io/posts/2018-10-04-capability.html which would be for a new RIO project, as it uses GHC 8.6.

The environtment:
Supply environment according to: https://tech.fpcomplete.com/haskell/library/rio

Replace ExceptT by using exceptions:
-https://tech.fpcomplete.com/haskell/tutorial/exceptions
-https://tech.fpcomplete.com/haskell/library/rio

Replace StateT by using mutable variables: https://tech.fpcomplete.com/haskell/tutorial/mutable-variables
-}
module GMSH.ReaderT.State() where
