{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Queries.GameQueries where

--import Control.Applicative
import Control.Monad.IO.Class
import Data.ByteString.Base64
import Data.ByteString.UTF8
import Data.List
--import qualified Data.Aeson as Aeson

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

getGamesAPI :: IO FullGameSchedule
getGamesAPI = do
  un <- username
  pw <- password
  let encoded = encode . fromString $ un ++ ":" ++ pw
      authstr = (fromString "Basic ") `mappend` encoded
      request = setRequestHeaders [ ("Accept-Encoding", "gzip")
                                  , ("Authorization", authstr)
                                  ]
              $ fullSeasonGames
  response <- httpJSON request
  return $ getResponseBody response

filterUniqueTeams :: FullGameSchedule -> [Team]
filterUniqueTeams (FullGameSchedule gs) = nub $ map homeTeam gs

convertGameTimes :: FullGameSchedule -> [GameEntry]
convertGameTimes (FullGameSchedule gs) = gs

insertTeamsMongo :: [Team] -> IO ()
insertTeamsMongo ts = do
  pipe <- connect (host "127.0.0.1")
  _ <- access pipe master "NBA2016-2017" (insertTeams ts)
  close pipe

insertGamesMongo :: [GameEntry] -> IO ()
insertGamesMongo gs = do
  pipe <- connect (host "127.0.0.1")
  _ <- access pipe master "NBA2016-2017" (insertGames gs)
  close pipe


insertTeams :: Control.Monad.IO.Class.MonadIO m => [Team] -> Action m [Value]
insertTeams ts = insertMany "teams" $ map makeTeamFields ts

insertGames :: Control.Monad.IO.Class.MonadIO m => [GameEntry] -> Action m [Value]
insertGames gs = insertMany "games" $ map makeGameFields gs


makeTeamFields :: Team -> [Field]
makeTeamFields t = [ "tid" =: tid t
                   , "city" =: city t
                   , "name" =: name t
                   , "abbreviation" =: abbreviation t]

makeGameFields :: GameEntry -> [Field]
makeGameFields g = [ "gid" =: eid g
                   , "date" =: date g
                   , "homeTeamID" =: (tid $ homeTeam g)
                   , "awayTeamID" =: (tid $ awayTeam g)]

