{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Queries.GameLogQueries where

--import Control.Concurrent.Async
import Control.Monad.IO.Class
import qualified Data.Bson as B
import Data.ByteString.Base64
import Data.ByteString.UTF8 hiding (foldl)
--import Data.Foldable (foldl)
--import Data.Monoid
import Database.MongoDB
import Models.BoxScores as BS hiding (game)
import Models.Games (eid)
import Models.TeamGameLogs -- TeamGameLogs | GameLog
import Network.HTTP.Simple
import Network.HTTP.Client (setQueryString)
import Queries.GameQueries (username, password)


gameLogReq :: Request
gameLogReq =
  "GET https://api.mysportsfeeds.com/v1.1/pull/nba/2016-2017-regular/team_gamelogs.json?"

buildGameLogQuery :: ByteString -> [(ByteString, Maybe ByteString)]
buildGameLogQuery abbr = [("team", Just abbr)]

--may have to use applicative: pull each query, execute it and tally at the end.
getOneGameLogAPI :: [(ByteString, Maybe ByteString)] -> IO TeamGameLogs
getOneGameLogAPI query = do
  un <- username
  pw <- password
  let encoded = encode . fromString $ un ++ ":" ++ pw
      authstr = (fromString "Basic ") `mappend` encoded
      request = setRequestHeaders [ ("Accept-Encoding", "gzip")
                                  , ("Authorization", authstr)
                                  ]
              $ setQueryString query gameLogReq
  response <- httpJSON request
  return $ getResponseBody response

--we need the games so we can get the ids:
getAllTeamsMongo :: Action IO [Document]
getAllTeamsMongo = rest =<< find (select [] "teams")

accumulateTeams :: Action IO [Document] -> Action IO [String]
accumulateTeams mdocs = (map (B.lookup "abbreviation")) <$> mdocs

allAbbreviations :: Action IO [String]
allAbbreviations = accumulateTeams getAllTeamsMongo

--only thing that didn't need to be done concurrently really.
buildGameLogReqStrs :: IO [[(ByteString, Maybe ByteString)]]
buildGameLogReqStrs = do
  pipe <- connect (host "127.0.0.1")
  abbrs <- access pipe master "NBA2016-2017" allAbbreviations
  return $ map (buildGameLogQuery . fromString) abbrs

makeGameLogReqs :: IO [TeamGameLogs]
makeGameLogReqs = do
  qs <- buildGameLogReqStrs
  gl <- traverse getOneGameLogAPI qs
  return gl

leapGameLogs :: [TeamGameLogs] -> [[GameLog]]
leapGameLogs = map gameLogs
-- makeReqs >>= 
-- combineBoxScores :: TeamGameLogs -> [StatsWithGameID]
-- combineBoxScores bs =
--   foldl (\a -> \b -> (<>) [StatsWithGameID (eid $ game b) (awayTeam b)
--                           , StatsWithGameID (eid $ game b) (homeTeam b)] a) [] bs

insertGameLogsMongo :: [[GameLog]] -> IO ()
insertGameLogsMongo ts = do
  pipe <- connect (host "127.0.0.1")
  _ <- access pipe master "NBA2016-2017" (traverse insertGameLogs ts)
  close pipe

insertGameLogs :: MonadIO m => [GameLog] -> Action m [Value]
insertGameLogs ts = insertMany "boxscores" $ map makeGameLogFields ts

makeGameLogFields :: GameLog -> [Field]
makeGameLogFields gl =
                     [ "gid" =: (eid $ game gl)
                     , "fg2PtAtt" =: (read $ fg2PtAtt $ stats gl :: Float)
                     , "fg2PtMade" =: (read $ fg2PtMade $ stats gl :: Float)
                     , "fg3PtAtt" =: (read $ fg3PtAtt $ stats gl :: Float)
                     , "fg3PtMade" =: (read $ fg3PtMade $ stats gl :: Float)
                     , "ftAtt" =: (read $ ftAtt $ stats gl :: Float)
                     , "ftMade" =: (read $ ftMade $ stats gl :: Float)
                     , "offReb" =: (read $ offReb $ stats gl :: Float)
                     , "defReb" =: (read $ defReb $ stats gl :: Float)
                     , "ast" =: (read $ ast $ stats gl :: Float)
                     , "pts" =: (read $ pts $ stats gl :: Float)
                     , "tov" =: (read $ tov $ stats gl :: Float)
                     , "stl" =: (read $ stl $ stats gl :: Float)
                     , "blk" =: (read $ blk $ stats gl :: Float)
                     , "fouls" =: (read $ fouls $ stats gl :: Float)
                     , "techFouls" =: (read $ techFouls $ stats gl :: Float)
                     , "plusMinus" =: (read $ plusMinus $ stats gl :: Float)]

