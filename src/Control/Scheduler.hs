{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}

module Control.Scheduler (
  Status(..),
  ExecutionResult(..),
  Timer(..),
  JobState(..),
  ScheduledJob(..),
  runScheduler,
  submitJob,
  lift,
  whenReady,
  whenReadyS,
  onStateChange
) where

import           Control.Concurrent        (threadDelay)
import           Control.Monad             (when)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.State       (MonadState, StateT, evalStateT, get,
                                            lift, modify, put)
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Data.Maybe                (fromMaybe)
import qualified Data.PQueue.Prio.Min      as PQ
import           Data.Time.Clock.POSIX     (POSIXTime, getPOSIXTime)

data Status = Ok | Error deriving (Eq, Show)
data ExecutionResult = Continue Status | Halt deriving (Eq, Show)

data ScheduledJob jobtype =
  Periodic { runInterval :: Integer, jobdata :: jobtype }
  | Once { atTime :: POSIXTime, jobdata :: jobtype }
  deriving (Eq, Show, Ord)

data JobState jobtype = JobState {
  jobDefinition :: !(ScheduledJob jobtype),
  lastCompleted :: !(Maybe POSIXTime),
  lastStatus    :: !(Maybe Status),
  lastWakeup    :: !(Maybe POSIXTime)
} deriving (Eq, Show)

data SchedulerState jobtype m = SchedulerState {
  jobQueue            :: !(PQ.MinPQueue POSIXTime (JobState jobtype)),
  offsetReference     :: !POSIXTime,
  offsetPeriod        :: !POSIXTime,
  stateChangeCallback :: !(Maybe (JobState jobtype -> m ()))
}

newtype Scheduler jobtype m a = Scheduler { unScheduler :: StateT (SchedulerState jobtype m) m a }
  deriving (Functor, Applicative, Monad, MonadState (SchedulerState jobtype m))

instance MonadTrans (Scheduler jobtype) where
  lift = Scheduler . lift

submitJob :: (Timer m) => ScheduledJob jobtype -> Scheduler jobtype m ()
submitJob job = do
  now <- currentTime

  let startTime = case job of
                    Periodic{}   -> now
                    Once{atTime} -> atTime

  modify $ \originalState@SchedulerState{jobQueue} ->
    originalState {
      jobQueue = PQ.insert startTime (JobState job Nothing Nothing Nothing) jobQueue
    }

runScheduler :: (Ord jobtype, Timer m) => [ScheduledJob jobtype] -> Scheduler jobtype m a -> m a
runScheduler jobs block = do
  now <- wholeSeconds <$> currentTime

  evalStateT (unScheduler actions) (initialState now)

  where
    actions = mapM_ submitJob jobs >> block

    initialState start = SchedulerState {
      jobQueue            = PQ.empty,
      offsetReference     = start,
      offsetPeriod        = fromIntegral beatPeriod,
      stateChangeCallback = Nothing
    }

    beatPeriod = foldl lcm 1 $ map period jobs
      where
        period Periodic{runInterval} = runInterval
        period Once{..}              = 1

    wholeSeconds :: POSIXTime -> POSIXTime
    wholeSeconds = fromIntegral . (floor :: POSIXTime -> Integer)

pullNext :: (Timer m) => Scheduler jobtype m (Maybe (JobState jobtype))
pullNext = do
  originalState@SchedulerState{..} <- get
  now <- currentTime

  case PQ.getMin jobQueue of
    Nothing -> return Nothing
    Just (wakeupTime, source) -> do
      let remainingJobs = PQ.deleteMin jobQueue

      if now >= wakeupTime
      then do
        put $ originalState { jobQueue = remainingJobs }
        return $ Just source { lastWakeup = Just now }
      else
        return Nothing

reschedule :: (Timer m) => JobState jobtype -> Status -> Scheduler jobtype m ()
reschedule JobState{jobDefinition = Once{..}} _ = return ()
reschedule jobState@JobState{jobDefinition = Periodic{..}} status = do
  now <- currentTime
  originalState@SchedulerState{stateChangeCallback, jobQueue, offsetReference} <- get

  let newJobState = jobState { lastCompleted = Just now, lastStatus = Just status }
      lastTime = fromMaybe now (lastWakeup jobState)
      nextTime = soonestAfter lastTime offsetReference timeIncrement

  put $ originalState { jobQueue = PQ.insert nextTime newJobState jobQueue }

  case stateChangeCallback of
    Nothing     -> return ()
    Just action -> lift $ action newJobState

  where
    timeIncrement = fromIntegral runInterval

sleep :: (Timer m) => Scheduler jobtype m () -> Scheduler jobtype m ()
sleep nextAction = do
  oldState@SchedulerState{..} <- get
  now <- currentTime

  case PQ.getMin jobQueue of
    Just (wakeupTime, _) -> do
      when (now > offsetReference + offsetPeriod) $
        put $ oldState { offsetReference = soonestAfter now offsetReference offsetPeriod }

      sleepUntil wakeupTime
      nextAction
    Nothing -> return ()

whenReady :: (Timer m) => (jobtype -> m ExecutionResult) -> Scheduler jobtype m ()
whenReady action = whenReadyS (lift . action)

whenReadyS :: (Timer m) => (jobtype -> Scheduler jobtype m ExecutionResult) -> Scheduler jobtype m ()
whenReadyS action =
  pullNext >>= \case
    Nothing ->
      sleep $ whenReadyS action
    Just readyJob ->
      action (jobdata . jobDefinition $ readyJob) >>= \case
        Continue result -> do
          reschedule readyJob result
          whenReadyS action
        Halt       -> return ()

onStateChange :: (Timer m) => (JobState jobtype -> m ()) -> Scheduler jobtype m ()
onStateChange action = modify $ \state -> state { stateChangeCallback = Just action }

soonestAfter :: POSIXTime -> POSIXTime -> POSIXTime -> POSIXTime
soonestAfter now baseTime increment =
  head $
    filter (> now) $
      iterate (+ increment) baseTime

class (Monad m) => Timer m where
  currentTime :: m POSIXTime
  timerSleep :: POSIXTime -> m ()

  sleepUntil :: POSIXTime -> m ()
  sleepUntil wakeupTime = do
    now <- currentTime
    timerSleep (wakeupTime - now)

instance (Timer m) => Timer (Scheduler jobtype m) where
  currentTime = lift currentTime
  timerSleep = lift . timerSleep

instance Timer IO where
  currentTime = getPOSIXTime
  timerSleep interval = threadDelay (round (1000 * 1000 * interval))

instance MonadIO (Scheduler jobtype IO) where
  liftIO = lift
