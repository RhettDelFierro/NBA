{-# LANGUAGE OverloadedStrings #-}
module Lib where

--import Database.MongoDB
import           Data.Aeson
import qualified Data.ByteString.Char8 as S8
import           Network.HTTP.Simple

import Models.Games


fullSeasonGames :: String
fullSeasonGames = "https://api.mysportsfeeds.com/v1.1/pull/nba/2016-2017-regular/full_game_schedule.json"

getTeams :: IO ()
getTeams = 

getGames :: IO ()
getGames = do
    request  <- parseRequest fullSeasonGames
    _ <- httpJSON request
    return ()
