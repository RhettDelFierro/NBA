{-# LANGUAGE OverloadedStrings #-}
module Models.Games where

import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format

data FullGameSchedule = FullGameSchedule [GameEntry] deriving (Show, Eq)

data GameEntry = GameEntry { eid :: String
                           , scheduleStatus :: String
                           , date :: UTCTime
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
    FullGameSchedule <$> ((o .: "fullgameschedule") >>= (.: "gameentry"))
  parseJSON _          = mzero

instance FromJSON GameEntry where
  parseJSON (Object o) =
    GameEntry <$> (o .: "id")
              <*> (o .: "scheduleStatus")
              <*> (parseGameTime <$> o .: "date")
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

parseGameTime :: String -> UTCTime
parseGameTime t =
  case parseTimeM True defaultTimeLocale "%F" t of
    Just d -> d
    Nothing -> error "could not parse date"
