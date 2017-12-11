module Main where

import Lib
import System.Environment (getEnv)


apiKey :: IO String
apiKey = getEnv "API_KEY"

main :: IO ()
main = do
  schedule <- getGamesAPI
  insertGamesMongo schedule
