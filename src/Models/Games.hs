--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE RecordWildCards #-}
module Models.Games where

import Control.Monad
import Data.Aeson
--import Data.Time.Clock
--import Data.Aeson.Types
--import qualified Data.Text as T

-- data DailyGameSchedule = DailyGameSchedule GameList

-- instance FromJSON DailyGameSchedule where
--   parseJSON (Object o) =
--     DailyGameSchedule <$> (o .: "dailygameschedule")
--   parseJSON _          = mzero

data FullGameSchedule = FullGameSchedule GameList deriving (Show, Eq)

newtype GameList = GameList [GameEntry] deriving (Show, Eq)

data GameEntry = GameEntry { eid :: String
                           , scheduleStatus :: String
                           , date :: String
                           , time :: String
                           , awayTeam :: Team
                           , homeTeam :: Team
                           , location :: String
                           } deriving (Show, Eq)

data Team = Team { tid :: String
                 , city :: String
                 , name :: String
                 , abbreviation :: String
                 } deriving (Show, Eq)

instance FromJSON FullGameSchedule where
  parseJSON (Object o) =
    FullGameSchedule <$> (o .: "fullgameschedule")
  parseJSON _          = mzero

instance FromJSON GameList where
  parseJSON (Object o) =
    GameList <$> (o .: "gameentry")
  parseJSON _          = mzero


instance FromJSON GameEntry where
  parseJSON (Object o) =
    GameEntry <$> (o .: "id")
              <*> (o .: "scheduleStatus")
              <*> (o .: "date")
              <*> (o .: "time")
              <*> (o .: "awayTeam")
              <*> (o .: "homeTeam")
              <*> (o .: "location")
  parseJSON _         = mzero

instance FromJSON Team where
  parseJSON (Object o) =
    Team <$> (o .: "ID")
         <*> (o .: "City")
         <*> (o .: "Name")
         <*> (o .: "Abbreviation")
  parseJSON _          = mzero
