{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Main where

import Platform


import Control.Monad
import Data.Int(Int64)
import Data.Word(Word32)
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
-- import qualified System.Time as ST
import qualified Data.Time.Clock.System as DT




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

data PInfo = PInfo !SP.ProcessHandle !DT.SystemTime
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


data Args =
  Args {
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
  sT <- now
  ec <- runProcess sT mv args
  eT <- now
  case ec of
    SE.ExitSuccess   -> hPutColoredLn SIO.stdout SCA.Green ("exited: 0")
    SE.ExitFailure c -> hPutColoredLn SIO.stdout SCA.Red   ("exited: " ++ show c ++ exit_extended_info)
      where c32 = fromIntegral c :: Word32

            exit_extended_info
              | c < -128 = TPF.printf " (0x%X" c32 ++ ext_info ++ ")"
              | otherwise = ""
              where ext_info =
#if mingw32_HOST_OS || mingw32_TARGET_OS
                      case c32 of
                        -- just an interesting subset
                        -- https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-erref/596a1078-e883-4972-9bbc-49e60bebca55
                        0x80000001 -> " STATUS_GUARD_PAGE_VIOLATION"
                        0x80000002 -> " STATUS_DATATYPE_MISALIGNMENT"
                        0x80000003 -> " STATUS_BREAKPOINT"
                        0x80000004 -> " STATUS_SINGLE_STEP"
                        0x80000005 -> " STATUS_BUFFER_OVERFLOW"
                        0x80000006 -> " STATUS_NO_MORE_FILES"
                        0x80000007 -> " STATUS_WAKE_SYSTEM_DEBUGGER"
                        0x8000000A -> " STATUS_HANDLES_CLOSED"
                        0x8000000B -> " STATUS_NO_INHERITANCE"
                        0x8000000D -> " STATUS_PARTIAL_COPY"
                        0x8000000E -> " STATUS_DEVICE_PAPER_EMPTY"
                        0x8000000F -> " STATUS_DEVICE_POWERED_OFF"
                        0x8000002B -> " STATUS_DLL_MIGHT_BE_INSECURE"
                        0x8000002C -> " STATUS_DLL_MIGHT_BE_INCOMPATIBLE"
                        0x80000289 -> " STATUS_DEVICE_DOOR_OPEN"
                        0x80010001 -> " DBG_EXCEPTION_NOT_HANDLED"
                        --
                        0xC0000005 -> " STATUS_ACCESS_VIOLATION"
                        0xC0000006 -> " STATUS_IN_PAGE_ERROR"
                        0xC0000007 -> " STATUS_PAGEFILE_QUOTA"
                        0xC0000008 -> " STATUS_INVALID_HANDLE"
                        0xC0000009 -> " STATUS_BAD_INITIAL_STACK"
                        0xC000000A -> " STATUS_BAD_INITIAL_PC"
                        0xC000000B -> " STATUS_INVALID_CID"
                        0xC000000C -> " STATUS_TIMER_NOT_CANCELED"
                        0xC000000D -> " STATUS_INVALID_PARAMETER"
                        0xC000000E -> " STATUS_NO_SUCH_DEVICE"
                        0xC000000F -> " STATUS_NO_SUCH_FILE"
                        0xC0000010 -> " STATUS_INVALID_DEVICE_REQUEST"
                        0xC0000011 -> " STATUS_END_OF_FILE"
                        0xC0000012 -> " STATUS_WRONG_VOLUME"
                        0xC0000013 -> " STATUS_NO_MEDIA_IN_DEVICE"
                        0xC0000014 -> " STATUS_UNRECOGNIZED_MEDIA"
                        0xC0000015 -> " STATUS_NONEXISTENT_SECTOR"
                        0xC0000016 -> " STATUS_MORE_PROCESSING_REQUIRED"
                        0xC0000017 -> " STATUS_NO_MEMORY"
                        0xC0000018 -> " STATUS_CONFLICTING_ADDRESSES"
                        0xC0000019 -> " STATUS_NOT_MAPPED_VIEW"
                        0xC000001A -> " STATUS_UNABLE_TO_FREE_VM"
                        0xC000001B -> " STATUS_UNABLE_TO_DELETE_SECTION"
                        0xC000001C -> " STATUS_INVALID_SYSTEM_SERVICE"
                        0xC000001D -> " STATUS_ILLEGAL_INSTRUCTION"
                        0xC000001E -> " STATUS_INVALID_LOCK_SEQUENCE"
                        --
                        0xC0000022 -> " STATUS_NONCONTINUABLE_EXCEPTION"
                        --
                        0xC0000025 -> " STATUS_NONCONTINUABLE_EXCEPTION"
                        --
                        0xC000002B -> " STATUS_PARITY_ERROR"
                        --
                        0xC0000032 -> " STATUS_DISK_CORRUPT_ERROR"
                        0xC0000039 -> " STATUS_OBJECT_PATH_INVALID"
                        0xC000003A -> " STATUS_OBJECT_PATH_NOT_FOUND"
                        0xC000003B -> " STATUS_OBJECT_PATH_SYNTAX_BAD"
                        0xC000003E -> " STATUS_DATA_ERROR"
                        --
                        0xC0000093 -> " STATUS_FLOAT_UNDERFLOW"
                        0xC0000094 -> " STATUS_INTEGER_DIVIDE_BY_ZERO"
                        0xC0000095 -> " STATUS_INTEGER_OVERFLOW"
                        0xC0000096 -> " STATUS_PRIVILEGED_INSTRUCTION"
                        --
                        0xC00000B0 -> " STATUS_PIPE_DISCONNECTED"
                        --
                        0xC00000B5 -> " STATUS_IO_TIMEOUT"
                        --
                        0xC00000BB -> " STATUS_NOT_SUPPORTED"
                        --
                        0xC000013A -> " STATUS_CONTROL_C_EXIT"
                        --
                        0xC0000142 -> " STATUS_DLL_INIT_FAILED"
                        --
                        0xC0000144 -> " STATUS_UNHANDLED_EXCEPTION"
                        --
                        0xC000014B -> " STATUS_PIPE_BROKEN"
                        _ -> ""
#else
                      -- TODO: unix behavior (e.g. report signal)
                      = ""
#endif

  when (argsTime args) $ do
    hPutColoredLn SIO.stdout SCA.White (TPF.printf "time: %7.3f s" (timeElapsedS eT sT))
  return ec

bUFFER_SIZE = 256 :: Int

runProcess :: DT.SystemTime -> PState -> Args -> IO SE.ExitCode
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

------------------------------------------------
------------------------ SIGNAL HANDLER
listProcessInfo :: PInfo -> IO ()
listProcessInfo (PInfo pH s) = do
  n <- now
  putStrLn (TPF.printf "\nruntime: %7.3f s\n" (timeElapsedS n s))
  listProcessInfoSys

-------
now :: IO DT.SystemTime
now = DT.getSystemTime

timeS :: IO a -> IO (a,Double)
timeS ioa = do
  ts <- DT.getSystemTime
  a <- ioa
  te <- DT.getSystemTime
  return (a,te`timeElapsedS`ts)

timeElapsedS :: DT.SystemTime -> DT.SystemTime -> Double
timeElapsedS te ts = fromIntegral (timeElapsedNS te ts) / 1e9
  where stToS t =
          fromIntegral (DT.systemSeconds t) +
            fromIntegral (DT.systemNanoseconds t) / 1e9

timeElapsedNS :: DT.SystemTime -> DT.SystemTime -> Int64
timeElapsedNS te ts = stToNanos te - stToNanos ts
  where stToNanos t = (DT.systemSeconds t)*1000*1000*1000 + fromIntegral (DT.systemNanoseconds t)

--
-- old System.Time approach
-- nowS :: IO Double
-- nowS = fmap (\i64 -> fromIntegral i64 / 1000000.0) nowMicros
--
-- nowMicros :: IO Int64
-- nowMicros = do
--   (ST.TOD s picos) <- ST.getClockTime
--   return $! (fromInteger s * 1000 * 1000 + (fromInteger picos `div` 1000000))


