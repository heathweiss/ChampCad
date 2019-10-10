{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Provides the environment for the ReaderT pattern as provided by the RIO module.
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
module GMSH.RIO.State(newBuilderEnv) where

import RIO
import qualified RIO.HashMap as HM

import Prelude (putStrLn) 

import qualified TypeClasses.Showable as Showable

import Data.Data
import Data.Typeable
import qualified Data.IORef as IORef
import qualified Data.HashMap.Strict as HM

{- |
The automatically generated ID that identifies each CurvePoint.
See GMSH.CurvePoints for CurvePoints information.
The constructor does not get exported, to ensure that the CurvePointId only gets generated through this module.
-}
newtype CurvePointId = CurvePointId {_curvePointId :: Int -- ^ The Id.
                                    }
 deriving (Show, Eq, Typeable, Data)
instance Showable.Showable CurvePointId

{- |
The automatically generated ID that identifies each Curve.
See GMSH.Curve for Curve information.
The constructor does not get exported, to ensure that the CurveId only gets generated through this module.
-}
newtype CurveId = CurveId {_curveId :: Int}
  deriving (Eq, Typeable, Data)

instance Show CurveId where
  show (CurveId curveId) =  show curveId


{- |
Supplies the environment for the RIO pattern.
-}
data BuilderEnv = BuilderEnv
                     { curvePointsMap :: !(IORef.IORef (HashMap Int CurvePointId)), -- ^ Hashmap used to ensure each Point only gets save once, as a unique CurvePoint.
                       
                       curveIdSupply :: !(IORef.IORef CurveId), -- ^ Supply id's for Curves.
                       
                       curvePointsIdSupply :: !(IORef.IORef CurvePointId) -- ^ Supply id's for CurvePoints.
                       
                     }


-- | Build a new BuilderEnv from the input values.
-- | The values all need to be supplied, as they are IORef's, and they need to be declared in the IO container.
newBuilderEnv :: (IORef.IORef (HashMap Int CurvePointId)) -> (IORef.IORef CurveId) -> (IORef.IORef CurvePointId) ->  BuilderEnv
newBuilderEnv  hashMap curveIdSupply curvePointIdSupply = BuilderEnv hashMap curveIdSupply curvePointIdSupply
  
  

class HasCurveIdSupply env where
  curveIdSupplyL :: Lens' env (IORef CurveId)
  --showCurveIdSupply :: env -> IO (Text)
  --showCurveIdSupply env' = readIORef $ curveIdSupplyL env'

instance HasCurveIdSupply BuilderEnv where
  curveIdSupplyL = lens curveIdSupply (\x y -> x { curveIdSupply = y })
{-
viewCurveIdSupply :: HasCurveIdSupply env => RIO env (String)
viewCurveIdSupply = do
  curveIdSupply <- view curveIdSupplyL
  let reader :: IORef (CurveId) -> IO (String)
      reader ref = do
        putStrLn $ show $ readIORef ref
        return $ show $ readIORef ref
  liftIO $ reader curveIdSupply
  --liftIO $ putStrLn "temp"
-}

sayCurveIdSupply :: HasCurveIdSupply env => RIO env ()
sayCurveIdSupply = do
  h <- view curveIdSupplyL
  hRead <- readIORef h
  liftIO $ putStrLn $ show hRead

class (HasCurveIdSupply env) => SetCurveIdSupply env where
  setCurveIdSupply :: RIO env ()

instance SetCurveIdSupply BuilderEnv where
  setCurveIdSupply = do
    
    --liftIO $ writeIORef (view curveIdSupplyL) (CurveId 2)
    temp <- view curveIdSupplyL
    --let t = writeIORef (temp) (CurveId 2)
    t <- liftIO $ writeIORef (temp) (CurveId 2)
    liftIO $ putStrLn "help"
  
mainTest :: IO ()
mainTest = do
  curvePointsIdSupply'  <- IORef.newIORef $ CurvePointId 1
  curveIdSupply' <- IORef.newIORef $ CurveId 1
  pointsMap <- IORef.newIORef $ HM.empty
  let env  = newBuilderEnv pointsMap curveIdSupply' curvePointsIdSupply'

  runRIO env $ do
    --curveId <- view curveIdSupplyL
    --curveIdRead <- readIORef curveId
    --liftIO $ putStrLn $ show curveIdRead
    sayCurveIdSupply
    liftIO $ putStrLn "done runRIO"


  --now look as env after RIO is done with it.
  curveIdRead <- readIORef $  curveIdSupply env
  putStrLn $ show curveIdRead
  
  putStrLn "done IO"
