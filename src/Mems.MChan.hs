module Mems.MChan
  ( MChan
  , newMChan
  , writeMChan
  , readMChan
  )
where

import Mems ( ECUCommand, ECUResponse )
import Control.Concurrent.STM.TBQueue
import Control.Monad.STM (atomically) -- , orElse)

-- | @MChan@ is a communication channel for MEMS.
data MChan = MChan (TBQueue ECUCommand)

-- | Builds and returns a new instance of @MChan@.
newMChan :: FilePath -> IO (MChan ECUCommand)
newMChan fp = do
    undefined
    atomically $ MChan <$> newTBQueue 10

-- | Writes a Command to @MChan@; blocks if the channel is full.
writeMChan :: BChan -> ECUCommand -> IO ()
writeMChan (BChan q) a = atomically $ writeTBQueue q a

-- | Reads the next value from the @MChan@; blocks if necessary.
readMChan :: BChan -> IO ECUResponse
readMChan (BChan q) = atomically $ readTBQueue q
