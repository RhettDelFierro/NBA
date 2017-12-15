{-# LANGUAGE OverloadedStrings #-}
module Models.BoxScores where

import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format
import Models.Games


data GameBoxScore = GameBoxScore { game :: GameEntry
                                 , quarterSummary :: QuarterSummary
                                 , awayTeam :: TeamStats
                                 , homeTeam :: TeamStats
                                 } deriving (Show, Eq)

data QuarterSummary = QuarterSummary { quarter :: Quarter
                                     , quarterTotals :: QuarterTotals
                                     } deriving (Show, Eq)

data Quarter = Quarter { number :: String
                       , awayScoreQT :: String
                       , homeScoreQT :: String
                       , scoring :: [ScoringPlay]
                       } deriving (Show, Eq)

newtype Scoring = Scoring [ScoringPlay] deriving (Show, Eq)

data ScoringPlay = ScoringPlay { time :: UTCTime
                               , teamAbbreviation :: String
                               , playDescription :: String
                               } deriving (Show, Eq)

data QuarterTotals = QuarterTotals { awayScore :: String
                                   , homeScore :: String } deriving (Show, Eq)

newtype AwayTeamStats = AwayTeamStats TeamStats deriving (Show, Eq)
newtype HomeTeamStats = HomeTeamStats TeamStats deriving (Show, Eq)
data TeamStats = TeamStats { fg2PtAtt  :: String
                           , fg2PtMade :: String
                           , fg3PtAtt  :: String
                           , fg3PtMade :: String
                           , ftAtt  :: String
                           , ftMade :: String
                           , offReb :: String
                           , defReb :: String
                           , ast :: String
                           , pts :: String
                           , tov :: String
                           , stl :: String
                           , blk :: String
                           , fouls :: String
                           , techFouls :: String
                           , plusMinus :: String
                           } deriving (Show, Eq)

instance FromJSON GameBoxScore where
  parseJSON (Object o) =
    GameBoxScore <$> ((o .: "gameboxscore") >>= (.: "game"))
                 <*> ((o .: "gameboxscore") >>= (.: "quarterSummary"))
                 <*> ((o .: "gameboxscore") >>= (.: "awayTeam"))
                 <*> ((o .: "gameboxscore") >>= (.: "homeTeam"))
  parseJSON _          = mzero

instance FromJSON QuarterSummary where
  parseJSON (Object o) =
    QuarterSummary <$> (o .: "quarter") <*> (o .: "quarterTotals")
  parseJSON _         = mzero

instance FromJSON Quarter where
  parseJSON (Object o) =
    Quarter <$> (o .: "@number")
         <*> (o .: "awayScore")
         <*> (o .: "homeScore")
         <*> ((o .: "scoring") >>= (.: "scoringPlay"))
  parseJSON _          = mzero

instance FromJSON ScoringPlay where
  parseJSON (Object o) =
    ScoringPlay <$> (parseGameTimeQuarter <$> o .: "time")
                <*> (o .: "teamAbbreviation")
                <*> (o .: "playDescription")
  parseJSON _          = mzero

instance FromJSON QuarterTotals where
  parseJSON (Object o) =
    QuarterTotals <$> (o .: "awayScore")
                  <*> (o .: "homeScore")
  parseJSON _          = mzero

instance FromJSON TeamStats where
  parseJSON (Object o) =
    TeamStats <$> ((o .: "Fg2PtAtt")  >>= (.: "#text"))
              <*> ((o .: "Fg2PtMade") >>= (.: "#text"))
              <*> ((o .: "Fg3PtAtt")  >>= (.: "#text"))
              <*> ((o .: "Fg2PtMade") >>= (.: "#text"))
              <*> ((o .: "FtAtt") >>= (.: "#text"))
              <*> ((o .: "FtMade") >>= (.: "#text"))
              <*> ((o .: "OffReb") >>= (.: "#text"))
              <*> ((o .: "DefReb") >>= (.: "#text"))
              <*> ((o .: "Ast") >>= (.: "#text"))
              <*> ((o .: "Pts") >>= (.: "#text"))
              <*> ((o .: "Tov") >>= (.: "#text"))
              <*> ((o .: "Stl") >>= (.: "#text"))
              <*> ((o .: "Blk") >>= (.: "#text"))
              <*> ((o .: "Fouls") >>= (.: "#text"))
              <*> ((o .: "FoulTech") >>= (.: "#text"))
              <*> ((o .: "PlusMinus") >>= (.: "#text"))
  parseJSON _          = mzero

parseGameTimeQuarter :: String -> UTCTime
parseGameTimeQuarter t =
  case parseTimeM True defaultTimeLocale "%F" t of
    Just d -> d
    Nothing -> error "could not parse date"
