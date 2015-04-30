module Main where

import Control.Concurrent
import System.IO
import System.Random
import System.Environment

writeStream done  _ _ 0 = done >> return ()
writeStream done  h msg ttl = do
  r <- randomRIO (100, 200 :: Int)
  if r `mod` 2 == 0 then hPutStrLn h msg
    else hPutStr h msg
  hFlush h
  threadDelay (r * 1000)
  writeStream done h msg (ttl - 1)

main = do
  args <- getArgs
  n <- case args of
            [] -> return 16
            [x] -> return (read x :: Int)
            _ -> fail "\nusage: TestIO.exe [n=16]\n"
  mv <- newEmptyMVar
  tid <- forkOS (writeStream (putMVar mv ()) stdout "out" n)
  writeStream (return ()) stderr "err" n
  takeMVar mv -- ghetto thread join
  putStrLn "\nexiting"

