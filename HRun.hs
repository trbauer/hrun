{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Main where

import Control.Monad
import Data.Int(Int64)
import qualified GHC.ConsoleHandler as GHC
import qualified Foreign.Ptr as F
import qualified Foreign.Marshal.Alloc as F

import qualified Control.Concurrent.MVar as MVar

import qualified System.Process as SP
import qualified System.IO as SIO
import qualified System.Environment as SE
import qualified System.Exit as SE
import qualified Text.Printf as TPF

import qualified System.Console.ANSI as SCA -- from ansi-terminal-0.6
import qualified System.Time as ST

import Platform

-- TODO:
--   * control-break kills the child, need to block that signal somehow
--     - Print better process info (memory, dlls, etc...)
--     - Use dbghelp.dll to walk the stacks
--   * take snapshots as the process runs and record process info every few seconds
--       emit a console graph of the memory usage etc...

printUsage = putStrLn usage
usage =
  "hrun --- Prints the runtime of a command as well as coloring the output of stderr\n" ++
  "\n" ++
  "usage: hrun <options> <exec> <arg1> <arg2> ... <argn>\n" ++
  "where <options> are:\n" ++
  "  -c     color stderr\n" ++
  "  -s     treat the arguments as a shell command\n" ++
  "  -t     print run times\n" ++
  "\n" ++
  "  <exec> is the name of the excutable to run\n" ++
  "  <arg1> .. <argn> are the arguments to that process\n" ++
  "  No options are read after <exec> is matched\n" ++
  "  Control+C will dump intermediate information about the" ++
                          " process (Control+Break terminates).\n" ++
  "EXAMPLES:\n" ++
  "  hrun -c  foo.exe\n" ++
  "    * runs foo.exe with no arguments and colors stderr\n" ++
  "  hrun -ct foo.exe fooarg1 fooarg2\n" ++
  "    * runs foo.exe with args fooarg1 and fooarg2 and colors stderr\n" ++
  "  hrun -s \"foo.ex e| grep baz | head > myout\"\n" ++
  "    * runs the above as pipeline (-s needed for pipeline as well as redirect\n" ++
  "  hrun -s \"dir *.exe\"\n" ++
  "    * runs the above as pipeline (-s needed since dir is built-in and for the * expansion)\n"

main :: IO ()
main = do
  SE.getArgs >>= runWithArgs >>= SE.exitWith

-- Tests
t0 = runWithArgs ["ls","-la"]
t1 = runWithArgs ["TestIO.exe"]
t2 = runWithArgs ["-c","TestIO.exe"]
t3 = runWithArgs ["-c","-t","TestIO.exe"]
t4 = runWithArgs ["-s","ls *"]
t5 = runWithArgs ["-sc","ls *.hs"]
t6 = runWithArgs ["-s","ls -la | head -n 2"]
t7 = runWithArgs ["-t","false.exe"]
t8 = runWithArgs ["-t","true.exe"]

ts = sequence [t0,t1,t2,t3,t4,t5,t6,t7,t8]


-- Top-level interpreter-level API
runWithArgs :: [String] -> IO SE.ExitCode
runWithArgs args = do
  mv <- MVar.newEmptyMVar
  GHC.installHandler (GHC.Catch (consoleHandler mv))
  parseArgs 1 defaultArgs args >>= execute mv

data PInfo = PInfo !SP.ProcessHandle !Double
type PState = MVar.MVar PInfo

consoleHandler :: PState -> GHC.ConsoleEvent -> IO ()
-- consoleHandler mv GHC.Break = do
consoleHandler mv h
  | h == GHC.Break || h == GHC.ControlC = do
  mpH <- MVar.tryTakeMVar mv
  case mpH of
    Nothing -> return ()
    Just p -> listProcessInfo p >> MVar.putMVar mv p
consoleHandler _ _  = return ()



data Args = Args {
    argsColor :: Bool
  , argsTime :: Bool
  , argsShell :: Bool
  , argsExecFile :: FilePath
  , argsExecArgs :: [String]
  } deriving Show

defaultArgs = Args False False False "" []

parseArgs :: Int -> Args -> [String] -> IO Args
parseArgs _ args [] = invalidArg (-1) "expected executable"
parseArgs ix args (('-':opts):as)
  | null opts = invalidArg ix ("empty options")
  | length opts > 0 && head opts /= '-'
  = do
    args1 <- parseArgOpts ix args opts
    parseArgs (ix + 1) args1 as
parseArgs ix args (a:as)
  | a == "--help" = printUsage >> SE.exitSuccess
  | otherwise = return args {argsExecFile = a, argsExecArgs = as}  -- termination

parseArgOpts :: Int -> Args -> String -> IO Args
parseArgOpts _  args [] = return args
parseArgOpts ix args (o:os) = case o of
                                'c' -> parseArgOpts ix args{argsColor = True} os
                                'h' -> printUsage >> SE.exitSuccess
                                't' -> parseArgOpts ix args{argsTime = True} os
                                's' -> parseArgOpts ix args{argsShell = True} os
                                _ -> invalidArg ix ("invalid option: " ++ [o])

invalidArg :: Int -> String -> IO a
invalidArg ix s = putStderrLn (locStr ++ s) >> printUsage >> SE.exitFailure
  where locStr = if ix >= 0 then "arg "++show ix++": " else ""

fatal :: String -> IO a
fatal s = putStderrLn s >> SE.exitFailure

hPutColored, hPutColoredLn :: SIO.Handle -> SCA.Color -> String -> IO ()
hPutColored h c s = do
  SCA.hSetSGR h [SCA.SetColor SCA.Foreground SCA.Vivid c]
  SIO.hPutStr h s
  SCA.hSetSGR h [SCA.Reset]
hPutColoredLn h c s = do
  SCA.hSetSGR h [SCA.SetColor SCA.Foreground SCA.Vivid c]
  SIO.hPutStrLn h s
  SCA.hSetSGR h [SCA.Reset]

putStderr, putStderrLn :: String -> IO ()
putStderr = hPutColored SIO.stderr SCA.Red
putStderrLn = hPutColoredLn SIO.stderr SCA.Red
--  SCA.hSetSGR SIO.stderr [SCA.SetColor SCA.Foreground SCA.Vivid SCA.White]


execute :: PState -> Args -> IO SE.ExitCode
execute mv args = do
  -- print args
  sT <- nowS
  ec <- runProcess sT mv args
  eT <- nowS
  case ec of
    SE.ExitSuccess   -> hPutColoredLn SIO.stdout SCA.Green ("exited: 0")
    SE.ExitFailure c -> hPutColoredLn SIO.stdout SCA.Red   ("exited: " ++ show c ++ (if c < -128 then TPF.printf " (0x%X)" c else ""))
  when (argsTime args) $ do
    hPutColoredLn SIO.stdout SCA.White (TPF.printf "time: %7.3f s" (eT - sT))
  return ec

bUFFER_SIZE = 256 :: Int
runProcess :: Double -> PState -> Args -> IO SE.ExitCode
runProcess s mv args = do
  let initExecArgs
        | argsShell args = SP.shell (unwords (argsExecFile args : argsExecArgs args))
        | otherwise      = SP.proc (argsExecFile args) (argsExecArgs args)
      pProcConfig
        | argsColor args  = initExecArgs {SP.std_err = SP.CreatePipe}
        | otherwise       = initExecArgs
  (Nothing, Nothing, mErr, pH) <- SP.createProcess (pProcConfig{SP.create_group = True})
  MVar.putMVar mv (PInfo pH s)
  case mErr of
    Nothing -> return ()
    Just hErr -> SIO.hSetBinaryMode SIO.stderr True >> F.allocaBytes bUFFER_SIZE (colorInput hErr)
  ec <- SP.waitForProcess pH
  return ec


colorInput :: SIO.Handle -> F.Ptr a -> IO ()
colorInput h ptr = do
  SIO.hWaitForInput h 100
  eof <- SIO.hIsEOF h
  if eof then return ()
    else do
      n <- SIO.hGetBufSome h ptr bUFFER_SIZE
      when (n > 0) $ do
        SCA.hSetSGR SIO.stderr [SCA.SetColor SCA.Foreground SCA.Vivid SCA.Red]
        SIO.hPutBuf SIO.stderr ptr n
        SCA.hSetSGR SIO.stderr [SCA.Reset]
      colorInput h ptr

nowS :: IO Double
nowS = fmap (\i64 -> fromIntegral i64 / 1000000.0) nowMicros

nowMicros :: IO Int64
nowMicros = do
  (ST.TOD s picos) <- ST.getClockTime
  return $! (fromInteger s * 1000 * 1000 + (fromInteger picos `div` 1000000))


------------------------------------------------
------------------------ SIGNAL HANDLER
listProcessInfo :: PInfo -> IO ()
listProcessInfo (PInfo pH s) = do
  now <- nowS
  putStrLn (TPF.printf "\nruntime: %7.3f s\n" (now - s))
  listProcessInfoSys

