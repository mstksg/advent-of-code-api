{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Advent.Throttle (
    Throttler
  , newThrottler
  , throttling
  , setLimit
  , getLimit
  ) where

import           Control.Concurrent
import           Control.Exception
import           Data.IORef

data Throttler = Throt { _throtSem     :: QSem
                       , _throtWaiting :: IORef Int
                       , _throtLim     :: IORef Int
                       }

acquireThrottler :: Throttler -> IO Bool
acquireThrottler Throt{..} = do
    currWait <- readIORef _throtWaiting
    throtLim <- readIORef _throtLim
    if currWait >= throtLim
      then pure False
      else do
        atomicModifyIORef' _throtWaiting ((,()) . (+1))
        waitQSem _throtSem `finally` atomicModifyIORef' _throtWaiting ((,()) . subtract 1)
        pure True

releaseThrottler :: Throttler -> IO ()
releaseThrottler Throt{..} = signalQSem _throtSem

-- | Set the maximum capacity of a 'Throttler'
setLimit :: Throttler -> Int -> IO ()
setLimit Throt{..} = atomicWriteIORef _throtLim

-- | Get the current maximum capacity of a 'Throttler'
getLimit :: Throttler -> IO Int
getLimit Throt{..} = readIORef _throtLim

-- | Create a new 'Throttler' with a given maximum capacity.
newThrottler :: Int -> IO Throttler
newThrottler n = do
    s <- newQSem 1
    w <- newIORef 0
    l <- newIORef n
    pure Throt
      { _throtSem     = s
      , _throtWaiting = w
      , _throtLim     = l
      }

-- | Perform an IO action with the given 'Throttler' and delay.  The IO
-- action will "wait in line" and be performed when the line is clear.  The
-- IO action will delay the next incoming IO action by the delay amount
-- given.
throttling
    :: Throttler
    -> Int                  -- ^ delay (in milliseconds)
    -> IO a
    -> IO (Maybe a)
throttling throt delay act = bracketOnError (acquireThrottler throt)
                                            (const (releaseThrottler throt)) $ \case
    False -> pure Nothing
    True  -> Just <$> do
      res <- act
      _ <- forkIO $ do
        threadDelay delay
        releaseThrottler throt
      pure res
