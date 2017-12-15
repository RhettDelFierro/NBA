{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Queries.BoxScoreQueries where

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
import Network.HTTP.Simple
import Network.HTTP.Client (setQueryString)
import Queries.GameQueries (username, password)

data StatsWithGameID = StatsWithGameID String TeamStats

singleBoxscoreReq :: Request
singleBoxscoreReq =
  "GET https://api.mysportsfeeds.com/v1.1/pull/nba/2016-2017-regular/game_boxscore.json?"

buildQuery :: ByteString -> [(ByteString, Maybe ByteString)]
buildQuery gameid = [("gameid", Just gameid)]

--may have to use applicative: pull each query, execute it and tally at the end.
getOneBoxScoreAPI :: [(ByteString, Maybe ByteString)] -> IO GameBoxScore
getOneBoxScoreAPI query = do
  un <- username
  pw <- password
  let encoded = encode . fromString $ un ++ ":" ++ pw
      authstr = (fromString "Basic ") `mappend` encoded
      request = setRequestHeaders [ ("Accept-Encoding", "gzip")
                                  , ("Authorization", authstr)
                                  ]
              $ setQueryString query singleBoxscoreReq
  response <- httpJSON request
  return $ getResponseBody response

--we need the games so we can get the ids:
getAllGamesMongo :: Action IO [Document]
getAllGamesMongo = rest =<< find (select [] "games")

accumulateGameIDS :: Action IO [Document] -> Action IO [String]
accumulateGameIDS mdocs = map (B.lookup "gid") <$> mdocs

allGameIds :: Action IO [String]
allGameIds = accumulateGameIDS  getAllGamesMongo

--only thing that didn't need to be done concurrently really.
buildReqStrs :: IO [[(ByteString, Maybe ByteString)]]
buildReqStrs = do
  pipe <- connect (host "127.0.0.1")
  ids <- access pipe master "NBA2016-2017" allGameIds
  return $ map (buildQuery . fromString) ids

makeReqs :: IO [StatsWithGameID]
makeReqs = do
  qs <- buildReqStrs
  bs <- mapConcurrently getOneBoxScoreAPI qs
  return $ combineBoxScores bs

combineBoxScores :: [GameBoxScore] -> [StatsWithGameID]
combineBoxScores bs =
  foldl (\a -> \b -> (<>) [StatsWithGameID (eid $ game b) (awayTeam b)
                          , StatsWithGameID (eid $ game b) (homeTeam b)] a) [] bs

insertBoxScoresMongo :: [StatsWithGameID] -> IO ()
insertBoxScoresMongo ts = do
  pipe <- connect (host "127.0.0.1")
  _ <- access pipe master "NBA2016-2017" (insertBoxScores ts)
  close pipe

insertBoxScores :: MonadIO m => [StatsWithGameID] -> Action m [Value]
insertBoxScores ts = insertMany "boxscores" $ map makeBoxScoreFields ts

makeBoxScoreFields :: StatsWithGameID -> [Field]
makeBoxScoreFields (StatsWithGameID gid ts) =
                     [ "gid" =: gid
                     , "fg2PtAtt" =: (read $ fg2PtAtt ts :: Float)
                     , "fg2PtMade" =: (read $ fg2PtMade ts :: Float)
                     , "fg3PtAtt" =: (read $ fg3PtAtt ts :: Float)
                     , "fg3PtMade" =: (read $ fg3PtMade ts :: Float)
                     , "ftAtt" =: (read $ ftAtt ts :: Float)
                     , "ftMade" =: (read $ ftMade ts :: Float)
                     , "offReb" =: (read $ offReb ts :: Float)
                     , "defReb" =: (read $ defReb ts :: Float)
                     , "ast" =: (read $ ast ts :: Float)
                     , "pts" =: (read $ pts ts :: Float)
                     , "tov" =: (read $ tov ts :: Float)
                     , "stl" =: (read $ stl ts :: Float)
                     , "blk" =: (read $ blk ts :: Float)
                     , "fouls" =: (read $ fouls ts :: Float)
                     , "techFouls" =: (read $ techFouls ts :: Float)
                     , "plusMinus" =: (read $ plusMinus ts :: Float)]

