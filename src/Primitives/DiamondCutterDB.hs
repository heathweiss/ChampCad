--for persist
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Primitives.DiamondCutterDB(uniqueDiamondName, DiamondDB(..), diamondDbToDiamondCutter, diamondDefaultDb) where

-- | Supply Persitence database functionality to DiamondCutter

import Primitives.DiamondCutter(DiamondBuilder(..), OffSet(..), defaultDiamondBuilder)

import CornerPoints.CornerPoints(CornerPoints(..))

-- for persist
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text.Internal

diamondDefaultDb :: Text
diamondDefaultDb = "src/Primitives/DiamondDefaults.sql" 

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DiamondDB
   diamondName String
   UniqueDiamondName diamondName
   desc String
   topHorizXOffset Double
   topHorizYOffset Double
   topHorizZOffset Double
   topVertXOffset  Double
   topVertYOffset  Double
   topVertZOffset  Double

   rightHorizXOffset Double
   rightHorizYOffset Double
   rightHorizZOffset Double
   rightVertXOffset  Double
   rightVertYOffset  Double
   rightVertZOffset  Double

   bottomHorizXOffset Double
   bottomHorizYOffset Double
   bottomHorizZOffset Double
   bottomVertXOffset  Double
   bottomVertYOffset  Double
   bottomVertZOffset  Double

   leftHorizXOffset Double
   leftHorizYOffset Double
   leftHorizZOffset Double
   leftVertXOffset  Double
   leftVertYOffset  Double
   leftVertZOffset  Double
   
  deriving Show
|]

uniqueDiamondName = UniqueDiamondName

initializeDatabase :: Text -> IO ()
initializeDatabase dbName = runSqlite dbName $ do
       
    runMigration migrateAll
    liftIO $ putStrLn "db initialized"

insertDiamond :: Text -> String -> String -> DiamondBuilder -> IO ()
insertDiamond dbName diamondName description (Diamond
                   _ _ _ _ topHorizontalOffset topVerticalOffset
                   _ _ _ _ rightHorizontalOffset rightVerticalOffset
                   _ _ _ _ bottomHorizontalOffset bottomVerticalOffset
                   _ _ _ _ leftHorizontalOffset leftVerticalOffset _)    = runSqlite dbName $ do
  diamondId 
    <- insert $
       DiamondDB
         diamondName
         description
         (xOffset topHorizontalOffset)
         (yOffset topHorizontalOffset)
         (zOffset topHorizontalOffset)
         
         (xOffset topVerticalOffset)
         (yOffset topVerticalOffset)
         (zOffset topVerticalOffset)
         
         (xOffset rightHorizontalOffset)
         (yOffset rightHorizontalOffset)
         (zOffset rightHorizontalOffset)
         
         (xOffset rightVerticalOffset)
         (yOffset rightVerticalOffset)
         (zOffset rightVerticalOffset)
         
         (xOffset bottomHorizontalOffset)
         (yOffset bottomHorizontalOffset)
         (zOffset bottomHorizontalOffset)

         (xOffset bottomVerticalOffset)
         (yOffset bottomVerticalOffset)
         (zOffset bottomVerticalOffset)
         
         (xOffset leftHorizontalOffset)
         (yOffset leftHorizontalOffset)
         (zOffset leftHorizontalOffset)

         (xOffset leftVerticalOffset)
         (xOffset leftVerticalOffset)
         (xOffset leftVerticalOffset)
       
  liftIO $ putStrLn "diamond inserted"

insertDefaultDiamond :: Text -> IO ()
insertDefaultDiamond dbName = 
  insertDiamond dbName "name me" "describe me"  $ defaultDiamondBuilder CornerPointsNothing
  


diamondDbToDiamondCutter :: DiamondDB -> DiamondBuilder
diamondDbToDiamondCutter
  (DiamondDB
   _ _
   topHorizXOffset
   topHorizYOffset
   topHorizZOffset
   topVertXOffset 
   topVertYOffset
   topVertZOffset

   rightHorizXOffset 
   rightHorizYOffset 
   rightHorizZOffset 
   rightVertXOffset  
   rightVertYOffset  
   rightVertZOffset  

   bottomHorizXOffset 
   bottomHorizYOffset 
   bottomHorizZOffset 
   bottomVertXOffset  
   bottomVertYOffset  
   bottomVertZOffset  

   leftHorizXOffset 
   leftHorizYOffset 
   leftHorizZOffset 
   leftVertXOffset  
   leftVertYOffset  
   leftVertZOffset  
  )
   =
    Diamond {   outerCube = CornerPointsNothing,
                topDiamondFace = Nothing,
                topDiamondCorner = Nothing,
                topCenterPoint = Nothing,
                topDiamondHorizontalOffsets = (OffSet topHorizXOffset topHorizYOffset topHorizZOffset),
                topDiamondVertiacalOffsets  = (OffSet topVertXOffset topVertYOffset topVertZOffset),
                topRightDiamondFace = Nothing,
                rightDiamondFace = Nothing,
                rightCenterPoint = Nothing,
                rightDiamondCorner = Nothing,
                rightDiamondHorizontalOffsets = (OffSet rightHorizXOffset rightHorizYOffset rightHorizZOffset),
                rightDiamondVerticalOffsets = (OffSet rightVertXOffset rightVertYOffset rightVertZOffset),
                bottomRightDiamondFace = Nothing,
                bottomDiamondFace = Nothing,
                bottomDiamondCorner = Nothing,
                bottomCenterPoint = Nothing,
                bottomDiamondHorizontalOffsets = (OffSet bottomHorizXOffset bottomHorizYOffset bottomHorizZOffset),
                bottomDiamondVerticalOffsets = (OffSet bottomVertXOffset bottomVertYOffset bottomVertZOffset),
                bottomLeftDiamondFace = Nothing,

                leftDiamondFace = Nothing,
                leftCenterPoint = Nothing,
                leftDiamondCorner = Nothing,
                leftDiamondHorizontalOffsets = (OffSet leftHorizXOffset leftHorizYOffset leftHorizZOffset),
                leftDiamondVerticalOffsets  = (OffSet leftVertXOffset leftVertYOffset leftVertZOffset),
                
                topLeftDiamondFace = Nothing
              }
      
