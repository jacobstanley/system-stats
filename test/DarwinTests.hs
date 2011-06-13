module Main (main) where

import Control.Applicative ((<$>))
import Foreign.Marshal (alloca, peekArray)
import Foreign.Storable (peek)

import System.Stats.CPU

main :: IO ()
main = do
    alloca $ \ppUsage -> do
    alloca $ \pCount -> do

    res <- c'get_cpu_usage ppUsage pCount

    pUsage <- peek ppUsage
    count <- fromInteger . toInteger <$> peek pCount

    usage <- peekArray count pUsage
    print usage

    c'free_cpu_usage pUsage
