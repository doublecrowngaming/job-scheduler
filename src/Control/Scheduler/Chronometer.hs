{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_HADDOCK hide           #-}

module Control.Scheduler.Chronometer (
  MonadChronometer(..),
  TimerResult(..),
  ChronometerT,
  runChronometerT,
  sendInterrupt,
  forkChronometerT
) where

import           Control.Concurrent           (ThreadId, forkIO, killThread,
                                               threadDelay)
import           Control.Concurrent.STM.TMVar
import           Control.Monad.Catch          (MonadCatch (..), MonadMask (..),
                                               MonadThrow (..))
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.IO.Unlift      (MonadUnliftIO, withRunInIO)
import           Control.Monad.Logger         (MonadLogger, MonadLoggerIO,
                                               logDebugNS)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.STM
import           Control.Monad.Trans.Class    (MonadTrans (..))
import           Control.Monad.Writer.Strict
import           Control.Scheduler.Time       (CurrentTime (..), Delay (..),
                                               ScheduledTime, diffTime)
import           Data.Time.Clock              (getCurrentTime)
import           Prometheus                   (MonadMonitor)


class Monad m => MonadChronometer m i | m -> i where
  now :: m CurrentTime
  at  :: ScheduledTime -> (TimerResult i -> m s) -> m s

instance MonadChronometer m i => MonadChronometer (ReaderT r m) i where
  now = lift now
  at time action = do
    r <- ask
    lift (at time $ \ x -> runReaderT (action x) r)

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

data TimerResult i = Interrupt i | Expiration

data ChronometerContext i = ChronometerContext {
  icTimer   :: Maybe ThreadId,
  icMailbox :: TMVar (TimerResult i)
}

newtype ChronometerT c io a = ChronometerT { unChronometerT :: StateT (ChronometerContext c) io a }
  deriving (
    Functor, Applicative, Monad, MonadIO,
    MonadThrow, MonadCatch, MonadMask,
    MonadLogger, MonadLoggerIO, MonadMonitor,
    MonadState (ChronometerContext c)
  )

instance MonadTrans (ChronometerT c) where
  lift = ChronometerT . lift

instance (MonadIO io, MonadUnliftIO io) => MonadChronometer (ChronometerT i io) i where
  now = liftIO (CurrentTime <$> getCurrentTime)

  at wakeupTime action = do
    startTimerThread wakeupTime

    timerResult <- liftIO . atomically . takeTMVar =<< gets icMailbox

    killTimerThread
    action timerResult

startTimerThread :: (MonadIO io, MonadUnliftIO io ) => ScheduledTime -> ChronometerT c io ()
startTimerThread wakeupTime =
  gets icTimer >>= \case
    Nothing -> do
      icTimer' <- forkChronometerT doTimer
      modify (\s -> s { icTimer = Just icTimer' })
    Just _ ->
      error "Attempt to start timer thread while one already exists"

  where
    timerSleep (Delay interval)
      | interval < 0 = return ()
      | otherwise    = threadDelay (1000 * 1000 * round interval)
    doTimer = do
      mbox <- gets icMailbox
      -- logDebugNS "ChronometerT" "Preparing to sleep"
      liftIO $ timerSleep . (wakeupTime `diffTime`) =<< CurrentTime <$> getCurrentTime
      -- logDebugNS  "ChronometerT" "Wakeup"
      liftIO $ atomically (putTMVar mbox Expiration)
      -- logDebugNS "ChronometerT" "Posted expiration"

killTimerThread :: MonadIO io => ChronometerT c io ()
killTimerThread =
  gets icTimer >>= \case
    Nothing -> return ()
    Just tid -> do
      liftIO $ killThread tid
      modify (\s -> s { icTimer = Nothing })

runChronometerT :: MonadIO io => ChronometerT c io a -> io a
runChronometerT action = do
  mailbox <- liftIO newEmptyTMVarIO

  evalStateT (unChronometerT action) $ ChronometerContext Nothing mailbox

sendInterrupt :: MonadIO io => c -> ChronometerT c io ()
sendInterrupt i = do
  ChronometerContext{..} <- get

  liftIO $ atomically (putTMVar icMailbox $ Interrupt i)

forkChronometerT :: (MonadIO io, MonadUnliftIO io) => ChronometerT c io () -> ChronometerT c io ThreadId
forkChronometerT (ChronometerT action) = do
  ctx <- get

  lift $
    withRunInIO $ \run ->
      liftIO . forkIO $ run (evalStateT action ctx)
