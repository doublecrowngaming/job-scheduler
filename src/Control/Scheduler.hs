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
  whenReady,
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

data ScheduledJob jobtype = Periodic { runInterval :: Integer, jobdata :: jobtype } deriving (Eq, Show, Ord)

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
    beatPeriod = foldl lcm 1 $ map runInterval jobs

    wholeSeconds :: POSIXTime -> POSIXTime
    wholeSeconds = fromIntegral . (floor :: POSIXTime -> Integer)

pullNext :: (Timer m) => Scheduler jobtype m (Maybe (JobState jobtype))
pullNext = do
  originalState@SchedulerState{..} <- get
  now <- currentTime

  let (wakeupTime, source) = PQ.findMin jobQueue
  let remainingJobs = PQ.deleteMin jobQueue

  if now >= wakeupTime
  then do
    put $ originalState { jobQueue = remainingJobs }
    return $ Just source { lastWakeup = Just now }
  else
    return Nothing

reschedule :: (Timer m) => JobState jobtype -> Status -> Scheduler jobtype m ()
reschedule jobState status = do
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
    timeIncrement = fromIntegral (runInterval . jobDefinition $ jobState)

sleep :: (Timer m) => Scheduler jobtype m ()
sleep = do
  oldState@SchedulerState{..} <- get

  let (wakeupTime, _) = PQ.findMin jobQueue

  now <- currentTime

  when (now > offsetReference + offsetPeriod) $
    put $ oldState { offsetReference = soonestAfter now offsetReference offsetPeriod }

  sleepUntil wakeupTime

whenReady :: (Timer m) => (jobtype -> m ExecutionResult) -> Scheduler jobtype m ()
whenReady action =
  pullNext >>= \case
    Nothing -> do
      sleep
      whenReady action
    Just readyJob ->
      lift (action $ jobdata . jobDefinition $ readyJob) >>= \case
        Continue result -> do
          reschedule readyJob result
          whenReady action
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
