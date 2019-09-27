{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import           Control.Concurrent           (ThreadId, forkIO, myThreadId,
                                               threadDelay)
import           Control.Concurrent.STM.TMVar
import           Control.Monad.Catch          (MonadCatch (..), MonadMask (..),
                                               MonadThrow (..))
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.IO.Unlift      (MonadUnliftIO (..),
                                               UnliftIO (..), withRunInIO)
import           Control.Monad.Logger         (MonadLogger, MonadLoggerIO,
                                               logDebugNS)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.STM
import           Control.Monad.Trans.Class    (MonadTrans (..))
import           Control.Monad.Writer.Strict
import           Control.Scheduler.Time       (CurrentTime (..), Delay (..),
                                               ScheduledTime, diffTime)
import           Data.Text                    (Text, pack)
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

newtype ChronometerContext i = ChronometerContext {
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

instance MonadUnliftIO m => MonadUnliftIO (ChronometerT c m) where
  askUnliftIO = do
    ctx <- get
    baseUnliftIO <- lift askUnliftIO
    return (UnliftIO $ \(ChronometerT actions) -> unliftIO baseUnliftIO $ evalStateT actions ctx)

instance (MonadIO io, MonadUnliftIO io, MonadLogger io) => MonadChronometer (ChronometerT i io) i where
  now = liftIO (CurrentTime <$> getCurrentTime)

  at wakeupTime action = do
    startTimerThread wakeupTime

    timerResult <- liftIO . atomically . takeTMVar =<< gets icMailbox

    action timerResult

startTimerThread :: (MonadIO io, MonadUnliftIO io, MonadLogger io) => ScheduledTime -> ChronometerT c io ()
startTimerThread wakeupTime =
  void $ forkChronometerT $ do
    tid  <- liftIO myThreadId
    mbox <- gets icMailbox

    let loc = "ChronometerT [" <> tshow tid <> "]"

    logDebugNS loc ("Preparing to sleep until " <> tshow wakeupTime)
    liftIO $ timerSleep . (wakeupTime `diffTime`) =<< CurrentTime <$> getCurrentTime

    logDebugNS loc ("Wakeup for " <> tshow wakeupTime)
    liftIO $ atomically (putTMVar mbox Expiration)

    logDebugNS loc ("Posted expiration for " <> tshow wakeupTime)

  where
    timerSleep (Delay interval)
      | interval < 0 = return ()
      | otherwise    = threadDelay (1000 * 1000 * round interval)

runChronometerT :: MonadIO io => ChronometerT c io a -> io a
runChronometerT action = do
  mailbox <- liftIO newEmptyTMVarIO

  evalStateT (unChronometerT action) $ ChronometerContext mailbox

sendInterrupt :: MonadIO io => c -> ChronometerT c io ()
sendInterrupt i = do
  ChronometerContext{..} <- get

  liftIO $ atomically . putTMVar icMailbox $ Interrupt i

forkChronometerT :: (MonadIO io, MonadUnliftIO io) => ChronometerT c io () -> ChronometerT c io ThreadId
forkChronometerT action =
  withRunInIO $ \run ->
    liftIO . forkIO $ run action

tshow :: Show a => a -> Text
tshow = pack . show
