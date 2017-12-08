{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Models.Games where


import GHC.Generics
import Data.Aeson
import Data.Time.Clock
import qualified Data.Text as T

data FullGameSchedule = FullGameSchedule { lastUpdatedOn :: UTCTime
                                         , gameEntry :: !Array } deriving (Show, Generic)

data GameEntry = GameEntry { eid :: Integer
                             , scheduleStatus :: T.Text
                             , date :: UTCTime
                             , time :: UTCTime
                             , awayTeam :: Team
                             , homeTeam :: Team
                             , location :: String
                             } deriving (Show, Generic, Eq)

data Team = Team { tid :: Integer
                 , city :: String
                 , name :: String
                 , abbreviation :: String
                 } deriving (Show, Generic, Eq)

instance FromJSON FullGameSchedule where
  parseJSON (Object o) = FullGameSchedule <$>
                         ((o .: "fullgameschedule") >>= (.: "lastUpdatedOn"))
                     <*> ((o .: "fullgameschedule") >>= (.: "gameentry"))
  parseJSON _          = mempty

instance FromJSON GameEntry where
  parseJSON (Object o) =
    GameEntry <$> (o .: "id")
              <*> (o .: "scheduleStatus")
              <*> (o .: "date")
              <*> (o .: "time")
              <*> (o .: "awayTeam")
              <*> (o .: "homeTeam")
              <*> (o .: "location")
  parseJSON _          = mempty

instance FromJSON Team
