{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Scheduler.Chronometer (
  MonadChronometer(..)
) where

import           Control.Concurrent               (threadDelay)
import           Control.Monad.Trans.Class        (MonadTrans (..))
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Writer
import           Control.Scheduler.Time           (CurrentTime (..), Delay (..),
                                                   ScheduledTime, diffTime)
import           Data.Time.Clock                  (getCurrentTime)

class Monad m => MonadChronometer m where
  now :: m CurrentTime
  at  :: ScheduledTime -> m s -> m s


instance MonadChronometer m => MonadChronometer (ReaderT r m) where
  now = lift now
  at time action = do
    r <- ask
    lift (at time $ runReaderT action r)

instance MonadChronometer m => MonadChronometer (StateT s m) where
  now = lift now
  at time action = do
    s <- get
    (a, s') <- lift (at time (runStateT action s))
    put s'
    return a

instance (Monoid w, MonadChronometer m) => MonadChronometer (WriterT w m) where
  now = lift now
  at time action = do
    (a, w) <- lift (at time (runWriterT action))
    tell w
    return a

instance MonadChronometer IO where
  now = CurrentTime <$> getCurrentTime

  at wakeupTime action = do
    timerSleep . (wakeupTime `diffTime`) =<< now
    action
      where
        timerSleep (Delay interval) = threadDelay (1000 * 1000 * round interval)
