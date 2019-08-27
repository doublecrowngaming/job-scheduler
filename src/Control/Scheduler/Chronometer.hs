{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_HADDOCK hide           #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE RecordWildCards            #-}

module Control.Scheduler.Chronometer (
  MonadChronometer(..),
  TimerResult(..),
  runInterruptable,
  runUninterruptable,
  sendInterrupt,
  forkInterruptable
) where

import           Control.Concurrent               (ThreadId, forkIO, killThread,
                                                   threadDelay)
import           Control.Concurrent.STM.TMVar
import           Control.Monad                    (void)
import           Control.Monad.Catch              (MonadCatch (..),
                                                   MonadMask (..),
                                                   MonadThrow (..))
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.IO.Unlift          (MonadUnliftIO, withRunInIO)
import           Control.Monad.Logger             (MonadLogger, MonadLoggerIO)
import           Control.Monad.STM
import           Control.Monad.Trans.Class        (MonadTrans (..))
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Writer
import           Control.Scheduler.Time           (CurrentTime (..), Delay (..),
                                                   ScheduledTime, diffTime)
import           Data.Maybe                       (isJust)
import           Data.Time.Clock                  (getCurrentTime)
import           Prometheus                       (MonadMonitor)

class Monad m => MonadChronometer m i | m -> i where
  now :: m CurrentTime
  at  :: ScheduledTime -> (TimerResult i -> m s) -> m s


instance MonadChronometer m i => MonadChronometer (ReaderT r m) i where
  now = lift now
  at time action = do
    r <- ask
    lift (at time $ \x -> runReaderT (action x) r)

instance MonadChronometer m i => MonadChronometer (StateT s m) i where
  now = lift now
  at time action = do
    s <- get
    (a, s') <- lift (at time (\x -> runStateT (action x) s))
    put s'
    return a

instance (Monoid w, MonadChronometer m i) => MonadChronometer (WriterT w m) i where
  now = lift now
  at time action = do
    (a, w) <- lift (at time (runWriterT . action))
    tell w
    return a

newtype Uninterruptable c io a = Uninterruptable { runUninterruptable :: io a }
  deriving (
    Functor, Applicative, Monad, MonadIO,
    MonadThrow, MonadCatch, MonadMask,
    MonadLogger, MonadLoggerIO, MonadMonitor
  )

instance MonadIO io => MonadChronometer (Uninterruptable i io) i where
  now = liftIO (CurrentTime <$> getCurrentTime)

  at wakeupTime action = do
    timerSleep . (wakeupTime `diffTime`) =<< now
    action Expiration
      where
        timerSleep (Delay interval) = liftIO $ threadDelay (1000 * 1000 * round interval)


data TimerResult i = Interrupt i | Expiration

newtype InterruptableContext i = InterruptableContext {
  icMailbox :: TMVar (TimerResult i)
}

newtype Interruptable c io a = Interruptable { unInterruptable :: ReaderT (InterruptableContext c) io a }
  deriving (
    Functor, Applicative, Monad, MonadIO,
    MonadThrow, MonadCatch, MonadMask,
    MonadLogger, MonadLoggerIO, MonadMonitor
  )

instance MonadIO io => MonadChronometer (Interruptable i io) i where
  now = liftIO (CurrentTime <$> getCurrentTime)

  at wakeupTime action = Interruptable $ do
    InterruptableContext{..} <- ask

    void . liftIO . forkIO $ do
      timerSleep . (wakeupTime `diffTime`) =<< CurrentTime <$> getCurrentTime
      atomically (putTMVar icMailbox Expiration)

    timerResult <- liftIO . atomically $ takeTMVar icMailbox

    unInterruptable $ action timerResult

    where
      timerSleep (Delay interval) = threadDelay (1000 * 1000 * round interval)

runInterruptable :: MonadIO io => Interruptable c io a -> io a
runInterruptable action = do
  mailbox <- liftIO newEmptyTMVarIO

  runReaderT (unInterruptable action) $ InterruptableContext mailbox

sendInterrupt :: MonadIO io => c -> Interruptable c io ()
sendInterrupt i = Interruptable $ do
  InterruptableContext{..} <- ask

  liftIO $ atomically (putTMVar icMailbox $ Interrupt i)

forkInterruptable :: (MonadIO io, MonadUnliftIO io) => Interruptable c io () -> Interruptable c io ThreadId
forkInterruptable (Interruptable action) = Interruptable $
  withRunInIO $ \run ->
    liftIO . forkIO $ run action
