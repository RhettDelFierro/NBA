{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Queries.GameQueries where

--import Control.Applicative
import Control.Monad.IO.Class
import Data.ByteString.UTF8
import Data.List

import Database.MongoDB
import Network.HTTP.Simple
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
  return . map homeTeam $ gameEntry $ getResponseBody response

insertTeamsMongo :: IO [Team] -> IO [GameEntry a] ->  IO ()
insertTeamsMongo teams games = do
  ts   <- nub <$> teams
  gs   <- games
  pipe <- connect (host "127.0.0.1")
  let f = access pipe master "NBA2016-2017"
  _    <- f (insertTeams ts)
  _    <- f (insertGames gs)
  close pipe
  
insertTeams :: Control.Monad.IO.Class.MonadIO m => [Team] -> Action m [Value]
insertTeams ts = insertMany "teams" $ brk ts
  where brk tms = map teamFields tms

insertGames :: Control.Monad.IO.Class.MonadIO m => [GameEntry a] -> Action m [Value]
insertGames gs = insertMany "games" $ brk gs
  where brk gms = map (\g -> [ "id" =:  eid g, "date" =: date g ]) gms


teamFields :: Team -> [Field]
teamFields = (\t -> [ "tid" =: tid t
                    , "city" =: city t
                    , "name" =: name t
                    , "abbreviation" =: abbreviation t])
