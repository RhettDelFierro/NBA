{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Queries.GameQueries where

import Control.Monad.IO.Class
import Data.ByteString.Base64
import Data.ByteString.UTF8
import Data.List

import Database.MongoDB
import Network.HTTP.Client (setQueryString)
import Network.HTTP.Simple
import System.Environment (getEnv)

import Models.Games

username :: IO String
username = getEnv "API_USERNAME"

password :: IO String
password = getEnv "API_PASSWORD"

fullSeasonGames :: Request
fullSeasonGames = "GET https://api.mysportsfeeds.com/v1.1/pull/nba/2016-2017-regular/full_game_schedule.json"

singleBoxscoreReq :: Request
singleBoxscoreReq = "GET https://api.mysportsfeeds.com/v1.1/pull/nba/2016-2017-regular/game_boxscore.json?"

buildQuery :: ByteString -> [(ByteString, Maybe ByteString)]
buildQuery gameid = [("gameid", Just gameid)]

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
                                  , ("force", "false")
                                  ]
              $ fullSeasonGames
  response <- httpJSON request
  return $ getResponseBody response

--may have to use applicative: pull each query, execute it and tally at the end.
getBoxScoreAPI :: IO FullGameSchedule
getBoxScoreAPI = do
  un <- username
  pw <- password
  let encoded = encode . fromString $ un ++ ":" ++ pw
      authstr = (fromString "Basic ") `mappend` encoded
      request = setRequestHeaders [ ("Accept-Encoding", "gzip")
                                  , ("Authorization", authstr)
                                  ]
              $ setQueryString (buildQuery "35144") singleBoxscoreReq
  response <- httpJSON request
  return $ getResponseBody response


filterUniqueTeams :: FullGameSchedule -> [Team]
filterUniqueTeams (FullGameSchedule gs) = nub $ map homeTeam gs

insertTeamsMongo :: [Team] -> IO ()
insertTeamsMongo ts = do
  pipe <- connect (host "127.0.0.1")
  _ <- access pipe master "NBA2016-2017" (insertTeams ts)
  close pipe

insertGamesMongo :: FullGameSchedule -> IO ()
insertGamesMongo fullgameschedule = do
  pipe <- connect (host "127.0.0.1")
  _ <- access pipe master "NBA2016-2017" (insertGames fullgameschedule)
  close pipe

insertTeams :: Control.Monad.IO.Class.MonadIO m => [Team] -> Action m [Value]
insertTeams ts = insertMany "teams" $ map makeTeamFields ts

insertGames :: Control.Monad.IO.Class.MonadIO m => FullGameSchedule -> Action m [Value]
insertGames (FullGameSchedule gs) = insertMany "games" $ map makeGameFields gs

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

