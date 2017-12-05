{-# LANGUAGE OverloadedStrings #-}
module Queries.GameQueries where

--import Database.MongoDB
--import           Data.Aeson
--import qualified Data.ByteString.Char8 as S8
import           Network.Wreq
import Control.Lens
import Data.Aeson (Value)
import Data.Aeson.Lens (key, nth)
import Data.ByteString.Internal
import System.Environment (getEnv)
import Models.Games

username :: IO String
username = getEnv "API_USERNAME"

password :: IO String
password = getEnv "API_PASSWORD"

fullSeasonGames :: String
fullSeasonGames = "https://api.mysportsfeeds.com/v1.1/pull/nba/2016-2017-regular/full_game_schedule.json"

getTeams :: IO ()
getTeams = undefined

getGames :: IO (Response Data.ByteString.Internal.ByteString)
getGames = do
   let un = username
       pw = password
       opts = defaults & header "Authorization" .~ "Basic " ++ un ++ ":" ++ pw
                       & header "Accept-Encoding" .~ ["gzip"]
   getWith opts fullSeasonGames
