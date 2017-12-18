{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Queries.GameLogsQueries where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import qualified Data.Bson as B
import Data.ByteString.Base64
import Data.ByteString.UTF8 hiding (foldl)
import Data.Foldable (foldl)
import Data.Monoid
import Database.MongoDB
import Models.BoxScores
import Models.Games (eid)
import Models.TeamGameLogs -- TeamGameLogs | GameLog
import Network.HTTP.Simple
import Network.HTTP.Client (setQueryString)
import Queries.GameQueries (username, password)

gameLogReq :: Request
gameLogReq =
  "GET https://api.mysportsfeeds.com/v1.1/pull/nba/2016-2017-regular/team_gamelogs.json?"

buildQuery :: ByteString -> [(ByteString, Maybe ByteString)]
buildQuery abbr = [("team", Just abbr)]

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
accumulateTeams mdocs = map (B.lookup "abbreviation") <$> mdocs

allAbbreviations :: Action IO [String]
allAbbreviations = accumulateTeams getAllTeamsMongo

--only thing that didn't need to be done concurrently really.
buildReqStrs :: IO [[(ByteString, Maybe ByteString)]]
buildReqStrs = do
  pipe <- connect (host "127.0.0.1")
  abbrs <- access pipe master "NBA2016-2017" allAbbreviations
  return $ map (buildQuery . fromString) abbrs

makeReqs :: IO [[GameLog]]
makeReqs = do
  qs <- buildReqStrs
  gl <- traverse getOneGameLogAPI qs
  return $ map gameLogs gl

-- combineBoxScores :: TeamGameLogs -> [StatsWithGameID]
-- combineBoxScores bs =
--   foldl (\a -> \b -> (<>) [StatsWithGameID (eid $ game b) (awayTeam b)
--                           , StatsWithGameID (eid $ game b) (homeTeam b)] a) [] bs

-- insertBoxScoresMongo :: [StatsWithGameID] -> IO ()
-- insertBoxScoresMongo ts = do
--   pipe <- connect (host "127.0.0.1")
--   _ <- access pipe master "NBA2016-2017" (insertBoxScores ts)
--   close pipe

-- insertBoxScores :: MonadIO m => [StatsWithGameID] -> Action m [Value]
-- insertBoxScores ts = insertMany "boxscores" $ map makeBoxScoreFields ts

-- makeBoxScoreFields :: StatsWithGameID -> [Field]
-- makeBoxScoreFields (StatsWithGameID gid ts) =
--                      [ "gid" =: gid
--                      , "fg2PtAtt" =: (read $ fg2PtAtt ts :: Float)
--                      , "fg2PtMade" =: (read $ fg2PtMade ts :: Float)
--                      , "fg3PtAtt" =: (read $ fg3PtAtt ts :: Float)
--                      , "fg3PtMade" =: (read $ fg3PtMade ts :: Float)
--                      , "ftAtt" =: (read $ ftAtt ts :: Float)
--                      , "ftMade" =: (read $ ftMade ts :: Float)
--                      , "offReb" =: (read $ offReb ts :: Float)
--                      , "defReb" =: (read $ defReb ts :: Float)
--                      , "ast" =: (read $ ast ts :: Float)
--                      , "pts" =: (read $ pts ts :: Float)
--                      , "tov" =: (read $ tov ts :: Float)
--                      , "stl" =: (read $ stl ts :: Float)
--                      , "blk" =: (read $ blk ts :: Float)
--                      , "fouls" =: (read $ fouls ts :: Float)
--                      , "techFouls" =: (read $ techFouls ts :: Float)
--                      , "plusMinus" =: (read $ plusMinus ts :: Float)]

