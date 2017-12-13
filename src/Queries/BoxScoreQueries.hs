{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Queries.BoxScoreQueries where

import Control.Concurrent.Async
import Data.ByteString.Base64
import Data.ByteString.UTF8
import Database.MongoDB
import Database.MongoDB (rest, find)
import Models.BoxScores
import Network.HTTP.Simple
import Network.HTTP.Client (setQueryString)
import Queries.GameQueries (username, password)

--get records from DB
--use this to perform queries on the urls:
--mapConcurrently :: Traversable t => (a -> IO b) -> t a -> IO (t b)
--put records into db from queries
--pages <- mapConcurrently getURL ["url1", "url2", "url3"]

singleBoxscoreReq :: Request
singleBoxscoreReq = "GET https://api.mysportsfeeds.com/v1.1/pull/nba/2016-2017-regular/game_boxscore.json?"

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

getAllGamesMongo :: Action IO [Document]
getAllGamesMongo = rest =<< find (select [] "games")
