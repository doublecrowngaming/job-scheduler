{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_HADDOCK hide           #-}

module Control.Scheduler.Chronometer (
  MonadChronometer(..)
) where

import           Control.Concurrent        (threadDelay)
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Scheduler.Time    (CurrentTime (..), Delay (..),
                                            ScheduledTime, diffTime)
import           Data.Time.Clock           (getCurrentTime)

class Monad m => MonadChronometer m where
  now        :: m CurrentTime
  sleepUntil :: ScheduledTime -> m ()

instance (MonadChronometer m, MonadTrans t, Monad (t m)) => MonadChronometer (t m) where
  now        = lift now
  sleepUntil = lift . sleepUntil

instance MonadChronometer IO where
  now = CurrentTime <$> getCurrentTime

  sleepUntil wakeupTime =
    timerSleep . (wakeupTime `diffTime`) =<< now
      where
        timerSleep (Delay interval) = threadDelay (1000 * 1000 * round interval)
