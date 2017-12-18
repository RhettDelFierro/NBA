{-# LANGUAGE OverloadedStrings #-}
module Models.TeamGameLogs where

import Control.Monad
import Data.Aeson
import Models.Games
import Models.BoxScores


newtype TeamGameLogs = TeamGameLogs { gameLogs :: [GameLog] } deriving (Show, Eq)

data GameLog = GameLog { game :: GameEntry
                       , team :: Team
                       , stats :: TeamStats
                       } deriving (Show, Eq)

instance FromJSON TeamGameLogs where
  parseJSON (Object o) =
    TeamGameLogs <$> ((o .: "teamgamelogs") >>= (.: "gamelogs"))
  parseJSON _          = mzero

instance FromJSON GameLog where
  parseJSON (Object o) =
    GameLog <$> (o .: "game") <*> (o .: "team") <*> (o .: "stats")
  parseJSON _          = mzero
