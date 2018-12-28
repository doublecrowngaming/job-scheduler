{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Scheduler (
  Status(..),
  ExecutionResult(..),
  Timer(..),
  JobState(..),
  ScheduledJob(..),
  ScheduledJobMatcher(..),
  Scheduler,
  Delay(..),
  Interval(..),
  runScheduler,
  submitJob,
  removeJob,
  clearJobs,
  lift,
  whenReady,
  whenReadyS,
  onStateChange
) where

import           Control.Concurrent        (threadDelay)
import           Control.Monad.Catch       (MonadCatch (..), MonadMask (..),
                                            MonadThrow (..))
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Logger      (MonadLogger (..))
import           Control.Monad.State       (MonadState, StateT, evalStateT, get,
                                            lift, modify, put)
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Scheduler.Time    (Delay (..), Interval (..),
                                            ReferenceTime (..), addDelay,
                                            diffTime, next, replaceTime)
import           Data.Maybe                (fromMaybe)
import qualified Data.PQueue.Prio.Min      as PQ
import           Data.Time.Clock           (DiffTime, UTCTime, getCurrentTime)

data Status = Ok | Error deriving (Eq, Show)
data ExecutionResult = Continue Status | Halt deriving (Eq, Show)

data ScheduledJob jobtype =
  Periodic        { runInterval :: Interval, jobdata :: jobtype }
  | PeriodicAfter { delay :: Delay, runInterval :: Interval, jobdata :: jobtype }
  | DailyAt       { time :: DiffTime, jobdata :: jobtype }
  | Once          { atTime :: UTCTime, jobdata :: jobtype }
  | Immediately   { jobdata :: jobtype }
  | After         { delay :: Delay, jobdata :: jobtype }
  deriving (Eq, Show)

data ScheduledJobMatcher jobtype =
  Exact (ScheduledJob jobtype)
  | ByContent jobtype

data JobState jobtype = JobState {
  jobDefinition :: !(ScheduledJob jobtype),
  lastCompleted :: !(Maybe UTCTime),
  lastStatus    :: !(Maybe Status),
  lastWakeup    :: !(Maybe UTCTime),
  referenceTime :: !ReferenceTime
} deriving (Eq, Show)

data SchedulerState jobtype m = SchedulerState {
  jobQueue            :: !(PQ.MinPQueue UTCTime (JobState jobtype)),
  stateChangeCallback :: !(Maybe (JobState jobtype -> m ()))
}

newtype Scheduler jobtype m a = Scheduler { unScheduler :: StateT (SchedulerState jobtype m) m a }
  deriving (Functor, Applicative, Monad, MonadState (SchedulerState jobtype m),
    MonadThrow, MonadCatch, MonadMask, MonadLogger, MonadIO)

instance MonadTrans (Scheduler jobtype) where
  lift = Scheduler . lift

submitJob :: (Timer m) => ScheduledJob jobtype -> Scheduler jobtype m ()
submitJob job = do
  now <- currentTime

  let startTime = case job of
                    Periodic{}           -> now
                    PeriodicAfter{delay} -> now `addDelay` delay
                    DailyAt{time}        -> now `replaceTime` time
                    Once{atTime}         -> atTime
                    Immediately{}        -> now
                    After{delay}         -> now `addDelay` delay
      referenceTime = ReferenceTime startTime

  modify $ \originalState@SchedulerState{jobQueue} ->
    originalState {
      jobQueue = PQ.insert startTime (JobState job Nothing Nothing Nothing referenceTime) jobQueue
    }

removeJob :: (Monad m, Eq jobtype) => ScheduledJobMatcher jobtype -> Scheduler jobtype m ()
removeJob toRemove = modify $ \SchedulerState{..} ->
  SchedulerState {
    jobQueue = PQ.filter (predicate toRemove) jobQueue,
    ..
  }
  where
    predicate (Exact scheduledJob) JobState{..} = jobDefinition /= scheduledJob
    predicate (ByContent jobData)  JobState{..} = jobdata jobDefinition /= jobData

clearJobs :: Monad m => Scheduler jobtype m ()
clearJobs = modify $ \originalState -> originalState { jobQueue = PQ.empty }

runScheduler :: (Timer m) => [ScheduledJob jobtype] -> Scheduler jobtype m a -> m a
runScheduler jobs block =

  evalStateT (unScheduler actions) initialState

  where
    actions = mapM_ submitJob jobs >> block

    initialState = SchedulerState {
      jobQueue            = PQ.empty,
      stateChangeCallback = Nothing
    }

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
reschedule jobState@JobState{jobDefinition, referenceTime} status' =
  case jobDefinition of
    Immediately{}              -> return ()
    Once{}                     -> return ()
    After{}                    -> return ()
    Periodic{runInterval}      -> reschedulePeriodic runInterval status'
    PeriodicAfter{runInterval} -> reschedulePeriodic runInterval status'
    DailyAt{}                  -> reschedulePeriodic (Interval 86400) status'
  where
    reschedulePeriodic runInterval status = do
      now <- currentTime
      originalState@SchedulerState{stateChangeCallback, jobQueue} <- get

      let newJobState = jobState { lastCompleted = Just now, lastStatus = Just status }
          lastTime = fromMaybe now (lastWakeup jobState)
          nextTime = next referenceTime runInterval lastTime

      put $ originalState { jobQueue = PQ.insert nextTime newJobState jobQueue }

      case stateChangeCallback of
        Nothing     -> return ()
        Just action -> lift $ action newJobState


sleep :: (Timer m) => Scheduler jobtype m () -> Scheduler jobtype m ()
sleep nextAction = do
  SchedulerState{..} <- get

  case PQ.getMin jobQueue of
    Just (wakeupTime, _) -> do
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

class (Monad m) => Timer m where
  currentTime :: m UTCTime
  timerSleep :: Delay -> m ()

  sleepUntil :: UTCTime -> m ()
  sleepUntil wakeupTime = do
    now <- currentTime
    timerSleep (wakeupTime `diffTime` now)

instance Timer IO where
  currentTime = getCurrentTime
  timerSleep (Delay interval) = threadDelay (1000 * 1000 * round interval)

instance (MonadTrans t, Timer m, (Monad (t m))) => Timer (t m) where
  currentTime = lift currentTime
  timerSleep = lift . timerSleep
