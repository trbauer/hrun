{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Platform where

import Control.Monad
import Foreign.Marshal
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import System.Win32.Types
import System.Win32.Process


#if mingw32_HOST_OS || mingw32_TARGET_OS

listProcessInfoSys :: IO ()
listProcessInfoSys = do
  putStrLn "------------------"
  mpid <- inferChildPid
  putStrLn $ "pid is " ++ show mpid
  case mpid of
    Just pid -> listProcessInfoOn pid
    Nothing -> return ()


inferChildPid :: IO (Maybe DWORD)
inferChildPid = withTh32Snap tH32CS_SNAPPROCESS Nothing $ \tsh -> do
  ps <- enumProcesses tsh
--  mapM_ print ps
  us <- getCurrentProcessId
  let child = filter (\(_,_,ppid,_,_) -> ppid == us) ps
  case child of
    [(pid,_,_,_,_)] -> return (Just pid)
    [] -> return Nothing




foreign import stdcall unsafe "windows.h GetCurrentProcessId"
  getCurrentProcessId :: IO DWORD



-- peLen :: Word32
-- peLen = 296
peLen = 260 * 2 + 9 * 4  -- MAX_PATH * sizeof(TCHAR) + 9 * 4-byte fields

-- | Enumerate processes using Process32First and Process32Next
enumProcesses :: Th32SnapHandle -> IO [ProcessEntry32]
enumProcesses h = allocaBytes ((556)) $ \pe -> do
{-# LINE 117 "libraries\Win32\System\Win32\Process.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) pe (((556))::DWORD)
{-# LINE 118 "libraries\Win32\System\Win32\Process.hsc" #-}
    ok <- c_Process32First h pe
    readAndNext ok pe []
    where
        readAndNext ok pe res
            | not ok    = do
                err <- getLastError
                if err == (18) -- no more files
{-# LINE 125 "libraries\Win32\System\Win32\Process.hsc" #-}
                    then return $ reverse res
                    else do
                      failWith "enumProcesses: Process32First/Process32Next" err
            | otherwise = do
                entry <- peekProcessEntry32 pe
                ok' <- c_Process32Next h pe
                readAndNext ok' pe (entry:res)


-- tH32CS_SNAPPROCESS
sNAP_FLAGS = tH32CS_SNAPTHREAD + tH32CS_SNAPMODULE + tH32CS_SNAPHEAPLIST

listProcessInfoOn :: DWORD -> IO ()
listProcessInfoOn pid = withTh32Snap sNAP_FLAGS (Just pid) $ \tsh -> do
  putStrLn "listProcessInfoOn:"
  ps <- enumProcesses tsh
  print ps

#endif


