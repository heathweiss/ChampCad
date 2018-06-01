{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ParallelListComp           #-}

module Persistable.Radial (Layer(..), AngleHeightRadius(..), AnglesHeightsRadii(..), nameUnique', angleHeightRadius', layerId',
                           angleHeightRadiusLayerId', extractAnglesHeightsRadiiFromEntity, ExtractedAngleHeightRadius(..),
                           extractRadii, extractAngles, extractHeights, extractLayerId, extractOrigin,
                           loadAndExtractedAngleHeightRadiusFromDB, loadAndExtractedAngleHeightRadiusFromDbT,
                           anglesHeightsRadiiToExtractedAnglesHeightsRadii, filterAngleHeightRadiusOnAngle) where

import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++),(+++>),(|+++|))
import CornerPoints.Radius (Radius(..))
import Geometry.Angle(Angle(..))

import qualified  Data.Text as T

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy hiding (get)
import qualified Control.Monad.State.Lazy as ST (get)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader

import TypeClasses.Transposable(TransposeLength, transpose)

{- | -------------------------overview---------------------------------------------
Create persist Datatypes that map [Radius] and [Angle] to a layer that has an associated origin.
This would be used by function such as CornerPoints.HorizontalFaces.CreateBottomFaces in order to create radial bottom faces.
This is the typical format used when capturing a shape radially, such as the shape of a shoe sole.

This is the database that gets used by the lineScanner.
The database structure will have to be extracted into the format req'd by functions such as CreateBottomFaces.
That is to say: Origin, [Radius], [Degree]
-}

{-
Layer: x y z makes up the origin of the surface.
Layer: name is the name of the layer being scanned. Eg: the sole of a shoe.

AngelRadius:
Correspond to the [Angle] and [Radius] that get passed into functions in CornerPoints.HorizontalFaces
for creating a radial layer.
-}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Layer
   NameUnique name
   name String
   x Double
   y Double
   z Double
  deriving Show

AngleHeightRadius
    angle Double
    height Double
    radius Double
    layerId LayerId
    deriving Show

|]

type Height = Double

type AnglesHeightsRadii = [AngleHeightRadius]

nameUnique' = NameUnique
angleHeightRadius' = AngleHeightRadius
layerId' = LayerId
angleHeightRadiusLayerId' = AngleHeightRadiusLayerId

databaseName = "src/Data/lineScanner.db"

-- | Create the database.
initializeDatabase :: IO ()
initializeDatabase = runSqlite databaseName $ do
       
    runMigration migrateAll
    liftIO $ putStrLn "db initializes"

insertLayer :: String -> IO ()
insertLayer layerName = runSqlite databaseName $ do
  layerId
       <- insert $ Layer
          --"layer name"
          layerName
          1
          2
          3
  liftIO $ putStrLn "layer inserted"


insertDegreeRadius :: String -> Double -> Double -> Double -> IO ()
insertDegreeRadius layerName angle height radius  = runSqlite databaseName $ do
  layerId <- getBy $ NameUnique layerName
  case layerId of
    (Just (Entity key val)) -> do
      degreeRadiusId <- insert $ AngleHeightRadius angle height radius key
      liftIO $ putStrLn "degree radius inserted"
    
    Nothing -> do
      liftIO $ putStrLn "degree radius not inserted"

-- | extract the x y z from layer as a Point
extractOrigin :: Layer -> Point
extractOrigin (Layer _ x y z) =
  Point x y z

-- | Extract the [Radius] from the AnglesHeightsRadii.
extractRadii :: AnglesHeightsRadii -> [Radius]
extractRadii angleHeightRadius =
  let
    extractRadius :: AngleHeightRadius -> Radius
    extractRadius (AngleHeightRadius _ _ r _) =
      Radius r
  in 
  map (extractRadius) angleHeightRadius


-- | Extract the [Angle] from the AnglesHeightsRadii.
extractAngles :: AnglesHeightsRadii -> [Angle]
extractAngles angleHeightRadius =
  let
    extractAngle :: AngleHeightRadius -> Angle
    extractAngle (AngleHeightRadius a _ _ _) =
      Angle a
  in 
  map (extractAngle) angleHeightRadius

-- | Extract the [Height] from the AnglesHeightsRadii.
extractHeights :: AnglesHeightsRadii -> [Height]
extractHeights angleHeightRadius =
  let
    extractHeight :: AngleHeightRadius -> Height
    extractHeight (AngleHeightRadius _ h _ _) =
       h
  in 
  map (extractHeight) angleHeightRadius

--should get rid of this
-- | Get the key from a Entity Layer selection.
extractLayerId :: Maybe (Entity Layer) -> Key Layer
extractLayerId (Just (Entity key val))  = key
--extractLayerId Nothing = 


-- | Extract the AnglesHeightsRadii from the Persist Entity AnglesHeightsRadii returned from selectList()
-- | 
extractAnglesHeightsRadiiFromEntity :: ([Entity AngleHeightRadius]) -> AnglesHeightsRadii
extractAnglesHeightsRadiiFromEntity anglesHeightsRadii =
  let
    extract :: (Entity AngleHeightRadius) -> AngleHeightRadius
    extract (Entity _ ahr) = ahr
  in
  map (extract) anglesHeightsRadii

-- |
-- Given:
--  filterFx :: (Double -> Bool) Fx to select a range of Doubles which represent the Angles.
--    Eg: <= 180 Eg: >= 90 & <= 270
--  ahr :: AnglesHeightsRadii. The AnglesHeightsRadii as loaded from the database.
-- Task:
--   Filter the AnglesHeightsRadii based on the angle.
-- Return:
--   The AnglesHeightsRadii as selected byt the 
filterAngleHeightRadiusOnAngle :: (Double -> Bool) ->  AnglesHeightsRadii -> AnglesHeightsRadii
filterAngleHeightRadiusOnAngle filterFx ahr =
  filter (\(AngleHeightRadius ang _ _ _) -> filterFx ang) ahr

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------NFG NFG NFG-------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
{-
This layer of abstraction caused lots of problems downstream and is not worth the bother. A lot of the problem is that
it puts us in the IO monad. Better just to use AngleHeightRadius.

Leave it in place for now, as that is how Examples.ShoeLift.MountainFlex.MountainFlexBase checked for ensuring ascending y_axis values.
It is a good reference for a similar system that will have to be built for AngleHeightRadius.
Once a new scan is done using an new AngleHeightRadius system, then all this can be deleted, as well as the work in Examples.ShoeLift.MountainFlex.MountainFlexBase.
-}

-- A handy wrapper used for extracting the [Angle], [Height], [Radius] the Persist layers.
-- Gets used to pass from the database extraction function to the Builder function
data ExtractedAngleHeightRadius = ExtractedAngleHeightRadius
                                   {_angleEAHR :: Angle,
                                    _heightEAHR :: Height,
                                    _radiusEAHR :: Radius
                                   }
                                   deriving (Show)

--will fail if layerName is not valid.
--Thought monad would protect from that.
loadAndExtractedAngleHeightRadiusFromDB :: String -> String -> IO (Maybe [ExtractedAngleHeightRadius])
loadAndExtractedAngleHeightRadiusFromDB  layerName dbName = runSqlite ( T.pack dbName) $ do
  layerId <- getBy $ nameUnique' layerName 
  

  case layerId of
    Nothing -> do
      liftIO $ putStrLn "layer was not found"
      return Nothing
    (Just (Entity key layer1AHR)) -> do
      liftIO $ putStrLn "layer was found"
      angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
      
      let ahr = extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity
      {-
          angles'  = extractAngles ahr
          heights' = extractHeights ahr
          radii'   = extractRadii ahr
      return $ Just
                 [ExtractedAngleHeightRadius a h r
                   | a <-angles'
                   | h <- heights'
                   | r <- radii'
                 ]-}
      return $ Just $ anglesHeightsRadiiToExtractedAnglesHeightsRadii ahr
{-
loadAndExtractedAngleHeightRadiusFromDB :: String -> String -> IO (Maybe [ExtractedAngleHeightRadius])
loadAndExtractedAngleHeightRadiusFromDB  layerName dbName = runSqlite ( T.pack dbName) $ do
  layerId <- getBy $ nameUnique' layerName 
  

  case layerId of
    Nothing -> do
      liftIO $ putStrLn "layer was not found"
      return Nothing
    (Just (Entity key layer1AHR)) -> do
      liftIO $ putStrLn "layer was found"
      angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
      let ahr = extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity
          angles'  = extractAngles ahr
          heights' = extractHeights ahr
          radii'   = extractRadii ahr
      return $ Just
                 [ExtractedAngleHeightRadius a h r
                   | a <-angles'
                   | h <- heights'
                   | r <- radii'
                 ]
-}
-- |
-- Given: ahr :: AnglesHeightsRadii, which is the Persistent datatype using Doubles to store Angles, Heights and Radii.  Has already been removed from the Persistent Entity
-- Task: Converts all the Doubles used by Persistent into the corresponding Angle and Radius. Height is left as a Double
-- Return: [ExtractedAngleHeightRadius] which is the same as the AnglesHeightsRadii but with Angle and Radius data types, as well as the Height::Double
anglesHeightsRadiiToExtractedAnglesHeightsRadii :: AnglesHeightsRadii -> [ExtractedAngleHeightRadius]
anglesHeightsRadiiToExtractedAnglesHeightsRadii ahr =
  let
    angles'  = extractAngles ahr
    heights' = extractHeights ahr
    radii'   = extractRadii ahr
  in
  [ExtractedAngleHeightRadius a h r
    | a <-angles'
    | h <- heights'
    | r <- radii'
  ]
 



{-
----------------------------------------------NFG-----------------------------------------------------
Will not compile unless I return a [] when layerId is Nothing.
Need to return Nothing.

Look at the monad-persist package which is is the Persistent mtl instance.
Maybe use in conjunction with: "build a haskell web api example by: lettier.github.io
-}
loadAndExtractedAngleHeightRadiusFromDbT :: String -> String -> MaybeT IO [ExtractedAngleHeightRadius]
loadAndExtractedAngleHeightRadiusFromDbT  layerName dbName = do
  n <-
   runSqlite ( T.pack dbName) $ do
     layerId <- getBy $ nameUnique' layerName
  
  
  
     case layerId of
       Nothing -> do
         --liftIO $ putStrLn "layer was not found"
         return [] -- works
         --Nothing --not work
         --return Nothing -- not work
         
       
       
       (Just (Entity key layer1AHR)) -> do
         liftIO $ putStrLn "layer was found"
         angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
         let ahr = extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity
             angles'  = extractAngles ahr
             heights' = extractHeights ahr
             radii'   = extractRadii ahr
         return $ 
                    [ExtractedAngleHeightRadius a h r
                      | a <-angles'
                      | h <- heights'
                      | r <- radii'
                    ]
 

  return n


runner = runMaybeT $  loadAndExtractedAngleHeightRadiusFromDbT "layer1" "src/Examples/ShoeLift/MountainFlex/ankleBrace.db"

instance TransposeLength AngleHeightRadius where
  transpose fx (AngleHeightRadius a h r i) =
    AngleHeightRadius a h (fx r) i
