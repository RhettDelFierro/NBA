module Main where

import Lib

main :: IO ()
main = do
  glss <- makeGameLogReqs
  insertGameLogsMongo $ leapGameLogs glss
  
