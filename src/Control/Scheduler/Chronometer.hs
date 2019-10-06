{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_HADDOCK hide           #-}

module Control.Scheduler.Chronometer (
  MonadChronometer(..),
  TimerResult(..),
  ChronometerT,
  runChronometerT,
  runLoggingChronometerT,
  sendInterrupt,
  forkChronometerT
) where

import           Control.Concurrent           (ThreadId, forkIO, threadDelay)
import           Control.Concurrent.STM.TMVar
import           Control.Monad.Catch          (MonadCatch (..), MonadMask (..),
                                               MonadThrow (..))
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.IO.Unlift      (MonadUnliftIO (..),
                                               UnliftIO (..), withRunInIO)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.STM
import           Control.Monad.Trans.Class    (MonadTrans (..))
import           Control.Monad.Writer.Strict
import           Control.Scheduler.Logging    (HasLogger (..), LogLevel (..),
                                               Logger, nullLogger, slog)
import           Control.Scheduler.Time       (CurrentTime (..), ScheduledTime,
                                               diffTime, toµsec)
import           Data.Text                    (Text, pack)
import           Data.Time.Clock              (getCurrentTime)

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
  icMailbox :: TMVar (TimerResult i),
  icLogger  :: Logger
}

newtype ChronometerT c io a = ChronometerT { unChronometerT :: ReaderT (ChronometerContext c) io a }
  deriving (
    Functor, Applicative, Monad, MonadIO,
    MonadThrow, MonadCatch, MonadMask,
    MonadReader (ChronometerContext c)
  )

instance MonadTrans (ChronometerT c) where
  lift = ChronometerT . lift

instance MonadUnliftIO m => MonadUnliftIO (ChronometerT c m) where
  askUnliftIO = do
    ctx <- ask
    baseUnliftIO <- lift askUnliftIO
    return (UnliftIO $ \(ChronometerT actions) -> unliftIO baseUnliftIO $ runReaderT actions ctx)

instance Monad m => HasLogger (ChronometerT c m) where
  getLogger = asks icLogger

instance (MonadIO io, MonadUnliftIO io) => MonadChronometer (ChronometerT i io) i where
  now = liftIO (CurrentTime <$> getCurrentTime)

  at wakeupTime action = do
    startTimerThread wakeupTime

    timerResult <- liftIO . atomically . takeTMVar =<< asks icMailbox

    action timerResult

startTimerThread :: (MonadIO io, MonadUnliftIO io) => ScheduledTime -> ChronometerT c io ()
startTimerThread wakeupTime =
  void $ forkChronometerT $ do
    mbox <- asks icMailbox

    slog DEBUG loc ("Preparing to sleep until " <> tshow wakeupTime)
    liftIO $ timerSleep . (wakeupTime `diffTime`) =<< CurrentTime <$> getCurrentTime

    slog DEBUG loc ("Wakeup for " <> tshow wakeupTime)
    liftIO $ atomically (putTMVar mbox Expiration)

    slog DEBUG loc ("Posted expiration for " <> tshow wakeupTime)

  where
    loc = "ChronometerT"
    timerSleep interval
      | interval < 0 = return ()
      | otherwise    = threadDelay $ toµsec interval

runChronometerT :: MonadIO io => ChronometerT c io a -> io a
runChronometerT = runLoggingChronometerT nullLogger

runLoggingChronometerT :: MonadIO io => Logger -> ChronometerT c io a -> io a
runLoggingChronometerT logger action = do
  mailbox <- liftIO newEmptyTMVarIO

  runReaderT (unChronometerT action) $ ChronometerContext mailbox logger

sendInterrupt :: MonadIO io => c -> ChronometerT c io ()
sendInterrupt i = do
  ChronometerContext{..} <- ask

  liftIO $ atomically . putTMVar icMailbox $ Interrupt i

forkChronometerT :: (MonadIO io, MonadUnliftIO io) => ChronometerT c io () -> ChronometerT c io ThreadId
forkChronometerT action =
  withRunInIO $ \run ->
    liftIO . forkIO $ run action

tshow :: Show a => a -> Text
tshow = pack . show
