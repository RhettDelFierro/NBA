{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Models.Games where


import GHC.Generics
import Data.Aeson
import Data.Time.Clock
import qualified Data.Text as T

data FullGameSchedule = FullGameSchedule {  gameEntry :: [GameEntry] } deriving (Show, Generic)

data GameEntry = GameEntry { eid :: Integer
                             , scheduleStatus :: T.Text
                             , date :: UTCTime
                             , time :: UTCTime
                             , awayTeam :: Team
                             , homeTeam :: Team
                             , location :: T.Text
                             } deriving (Show, Generic, Eq)

data Team = Team { tid :: Integer
                 , city :: T.Text
                 , name :: T.Text
                 , abbreviation :: T.Text
                 } deriving (Show, Generic, Eq)

instance FromJSON FullGameSchedule where
  parseJSON = withObject "fullgameschedule" $ \o -> do
    gameEntry <- o .:? "gameentry" .!= []
    return FullGameSchedule{..}

instance FromJSON GameEntry where
    parseJSON = withObject "gameentry" $ \o -> do
                         eid  <- o .: "id"
                         scheduleStatus <- o .: "scheduleStatus"
                         date <- o .: "date"
                         time <- o .: "time"
                         awayTeam <-  o .: "awayTeam"
                         homeTeam <-  o .: "homeTeam"
                         location <-  o .: "location"
                         return GameEntry{..}

instance FromJSON Team
