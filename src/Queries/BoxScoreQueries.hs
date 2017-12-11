{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Queries.BoxScoreQueries where

import Control.Concurrent.Async (async)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)

import Control.Concurrent
  (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM
import Prelude hiding (sum)
import qualified Data.List as L

sum :: [Int] -> Chan Int -> IO ()
sum xs chan = do
  let ret = L.sum xs
  writeChan chan ret

main :: IO ()
main = do
  let s = [7, 2, 8, -9, 4, 0]
  c <- newChan
  _ <- async $ sum (drop (length s `div` 2) s) c
  _ <- async $ sum (take (length s `div` 2) s) c
  {- `sequence` run a list of actions -}
  [x, y] <- sequence [readChan c, readChan c]
  print (x, y, x+y)

{- STM version -}

sumBySTM :: [Int] -> TQueue Int -> IO ()
sumBySTM xs chan = do
  let ret = L.sum xs
  atomically $ writeTQueue chan ret

mainBySTM :: IO ()
mainBySTM = do
  let s = [7, 2, 8, -9, 4, 0]
  c <- atomically $ newTQueue
  _ <- async $ sumBySTM (drop (length s `div` 2) s) c
  _ <- async $ sumBySTM (take (length s `div` 2) s) c
  [x, y] <- sequence [ atomically (readTQueue c)
                     , atomically (readTQueue c)]
  print (x, y, x+y)


{-
say :: String -> IO ()
say s = forM_ [0..4] $ \_ -> do
  threadDelay $ 100 * 10^3
  putStrLn s

main :: IO ()
main = do
  _ <- async $ say "world"
  say "hello"

  
-}
