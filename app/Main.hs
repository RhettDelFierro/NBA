module Main where

import Lib
import System.Environment (getEnv)

apiKey :: String
apiKey = getEnv "API_KEY"

main :: IO ()
main = someFunc
