{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}

module Persistable.Radial (Layer(..), AngleHeightRadius(..), AnglesHeightsRadii(..), nameUnique', angleHeightRadius', layerId',
                           angleHeightRadiusLayerId', extractAnglesHeightsRadiiFromEntity, ExtractedAngleHeightRadius(..),
                           extractRadii, extractAngles, extractHeights, extractLayerId, extractOrigin, loadAndExtractedAngleHeightRadiusFromDB) where

import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++),(+++>),(|+++|))
import CornerPoints.Radius (Radius(..))
import Geometry.Angle(Angle(..))

--import Data.Text

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


-- A handy wrapper used for extracting the [Angle], [Height], [Radius] the Persist layers.
-- Gets used to pass from the database extraction function to the Builder function
data ExtractedAngleHeightRadius = ExtractedAngleHeightRadius
                                   {angles :: [Angle],
                                    heights :: [Height],
                                    radii :: [Radius]
                                   }
                                   deriving (Show)

--will fail if layerName is not valid.
--Thought monad would protect from that.
loadAndExtractedAngleHeightRadiusFromDB :: String -> IO (ExtractedAngleHeightRadius)
loadAndExtractedAngleHeightRadiusFromDB  layerName = runSqlite databaseName $ do
  layerId <- getBy $ nameUnique' layerName --top = geox. bottom = golf
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  
  let ahr = extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity
      angles'  = extractAngles ahr
      heights' = extractHeights ahr
      radii'   = extractRadii ahr
  return $ ExtractedAngleHeightRadius angles' heights' radii'
