{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Models.Games where

import Control.Applicative
import GHC.Generics
import Data.Aeson
import Data.Time.Clock
import qualified Data.Text as T

data FullGameSchedule a = FullGameSchedule { lastUpdatedOn :: T.Text
                                           , gameEntry :: [a]
                                           } deriving (Show, Generic)

data GameEntry a = GameEntry { eid :: Integer
                             , scheduleStatus :: T.Text
                             , originalDate :: T.Text
                             , delayedOrPostponedReason :: T.Text
                             , date :: UTCTime
                             , time :: UTCTime
                             , awayTeam :: a
                             , homeTeam :: a
                             , location :: T.Text
                             } deriving (Show, Generic, Eq)

data Team = Team { tid :: Integer
                 , city :: T.Text
                 , name :: T.Text
                 , abbreviation :: T.Text
                 } deriving (Show, Generic, Eq)

instance (FromJSON a) => FromJSON (FullGameSchedule a) where
  parseJSON (Object v) = FullGameSchedule <$>
                         v .: "lastUpdatedOn" <*>
                         v .: "gameentry"
  parseJSON _        = empty

instance (FromJSON a) => FromJSON (GameEntry a) where
    parseJSON (Object v) = GameEntry <$>
                           v .: "id" <*>
                           v .: "scheduleStatus" <*>
                           v .: "originalDate" <*>
                           v .: "delaredOrPostponedReason" <*>
                           v .: "date" <*>
                           v .: "time" <*>
                           v .: "awayTeam" <*>
                           v .: "homeTeam" <*>
                           v .: "location"
    parseJSON _          = empty
    
instance FromJSON Team
