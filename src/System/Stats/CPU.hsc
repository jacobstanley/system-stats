{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

#ifdef darwin_HOST_OS
#include <bindings.dsl.h>
#include <darwin_cpu.h>
#endif

module System.Stats.CPU
    ( CPUStats (..)
    , getCPUStats

    , C'cpu_usage_t
    , c'get_cpu_usage
    , c'free_cpu_usage
    ) where

import           Control.Applicative ((<$>))
import           Control.Exception (bracket)
import           Data.Maybe (fromJust)
import           Data.List (find)
import           System.IO (openFile, hClose, IOMode(..))

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B


#ifdef darwin_HOST_OS

#strict_import

#starttype cpu_usage_t
#field user,   CUInt
#field system, CUInt
#field idle,   CUInt
#field nice,   CUInt
#stoptype

#ccall get_cpu_usage, Ptr (Ptr <cpu_usage_t>) -> Ptr CInt -> IO CInt

#ccall free_cpu_usage, Ptr <cpu_usage_t> -> IO ()

#endif

------------------------------------------------------------------------
-- Types

-- | Represents a percentage where 0% = 0.0 and 100% = 1.0
type Percentage = Double

-- | CPU usage statistics for a moment in time.
data CPUStats = CPUStats
    { cpuUser   :: Percentage -- ^ Time spent executing user processes
    , cpuSystem :: Percentage -- ^ Time spent executing system processes
    , cpuIdle   :: Percentage -- ^ Time spent doing nothing
    } deriving (Show)


------------------------------------------------------------------------
-- Top level functions

-- | Samples the CPU statistics for the current time.
getCPUStats :: IO CPUStats
#if defined(linux_HOST_OS)
getCPUStats = parseStat <$> readFile' "/proc/stat"
#else
getCPUStats = error "getCPUStats: not supported on this platform"
#endif

------------------------------------------------------------------------
-- Linux

#if defined(linux_HOST_OS)

parseStat :: ByteString -> CPUStats
parseStat = parseStatCPU . fromJust . (find (B.isPrefixOf "cpu ")) . B.lines

parseStatCPU :: ByteString -> CPUStats
parseStatCPU s = CPUStats
    { cpuUser   = (user + nice) / total
    , cpuSystem = sys / total
    , cpuIdle   = idle / total
    }
  where
    total = user + nice + sys + idle + iowait + hardirq + softirq

    (_:user:nice:sys:idle:iowait:hardirq:softirq:_) = map readNumber (B.words s)
    readNumber = fromInteger . toInteger . fst . fromJust . B.readInt


-- | Reads an entire file strictly into a 'ByteString'. This differs
-- from the 'B.readFile' shipped with the bytestring package in that it
-- supports reading special \"zero sized\" files like the ones linux
-- uses to report system information (eg. \/proc\/stat).
readFile' :: FilePath -> IO ByteString
readFile' f = bracket (openFile f ReadMode) hClose B.hGetContents

#endif


------------------------------------------------------------------------
-- OS/X

#if defined(darwin_HOST_OS)


#endif
