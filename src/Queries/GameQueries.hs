{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Queries.GameQueries where

--import Database.MongoDB
--import           Data.Aeson
--import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Simple
import Data.ByteString.UTF8
import System.Environment (getEnv)
import Models.Games

username :: IO String
username = getEnv "API_USERNAME"

password :: IO String
password = getEnv "API_PASSWORD"

fullSeasonGames :: Request
fullSeasonGames = "GET https://api.mysportsfeeds.com/v1.1/pull/nba/2016-2017-regular/full_game_schedule.json"

getTeams :: IO ()
getTeams = undefined

getGames :: IO (Response (FullGameSchedule (GameEntry Team)))
getGames = do
   un <- username
   pw <- password
   let authstr = fromString $ "Basic " ++ un ++ ":" ++ pw
       request = setRequestHeaders [ ("Accept-Encoding", "gzip")
                                   , ("Authorization", authstr)
                                   ]
               $ fullSeasonGames
   response <- httpJSON request
   return response
