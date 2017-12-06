{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Queries.GameQueries where

import Database.MongoDB
import Control.Monad.IO.Class
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

getGamesAPI :: IO [Team]
getGamesAPI = do
  un <- username
  pw <- password
  let authstr = fromString $ "Basic " ++ un ++ ":" ++ pw
      request = setRequestHeaders [ ("Accept-Encoding", "gzip")
                                  , ("Authorization", authstr)
                                  ]
              $ fullSeasonGames
  response <- httpJSON request
  return $ gameEntry (getResponseBody response)

insertTeamsMongo :: IO [Team] -> IO [GameEntry] ->  IO ()
insertTeamsMongo teams games = do
  ts <- teams
  ts <- games
  pipe <- connect (host "127.0.0.1")
  ts' <- access pipe master "NBA2016-2017" (insertTeams ts)
  gs  <- access pipe master "NBA2016-2017" (insertTeams gs)
  close pipe
  print ts'



insertTeams :: Control.Monad.IO.Class.MonadIO m => [Team] -> Action m [Value]
insertTeams ts = insertMany "games" $ brk ts
  where brk tms = map teamFields tms

teamFields :: Team -> [Field]
teamFields = (\t -> [ "tid" =: tid t
                    , "city" =: city t
                    , "name" =: name t
                    , "abbreviation" =: abbreviation t])
