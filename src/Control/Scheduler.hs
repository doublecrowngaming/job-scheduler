{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Scheduler where

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forM_, forever)
import           Control.Monad.Catch        (MonadCatch (..), MonadMask (..),
                                             MonadThrow (..))
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Strict (MonadState, StateT, evalStateT,
                                             get, gets, lift, modify, put)
import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Scheduler.Time     (Delay (..), Interval (..),
                                             ReferenceTime (..), addDelay,
                                             diffTime, next, replaceTime)
import           Data.Maybe                 (fromMaybe)
import qualified Data.PQueue.Prio.Min       as PQ
import           Data.Time.Clock            (DiffTime, UTCTime, getCurrentTime)
import           Numeric.Natural            (Natural)


data Status = Ok | Error deriving (Eq, Show)
data ExecutionResult = Continue Status | Halt deriving (Eq, Show)

class Monad m => MonadChronometer m where
  now :: m UTCTime
  sleepUntil :: UTCTime -> m ()

class Task t where
  type TaskData t :: *

  runAt   :: t -> UTCTime -> UTCTime
  nextJob :: t -> UTCTime -> Maybe (Job (TaskData t))
  apply :: t -> (TaskData t -> m a) -> m a

newtype Immediately d = Immediately d deriving (Eq, Show)

instance Task (Immediately d) where
  type TaskData (Immediately d) = d

  runAt   (Immediately _)   = id
  nextJob (Immediately _)   = const Nothing
  apply   (Immediately d) f = f d

class MonadChronometer m => MonadScheduler d m where
  schedule         :: (Task t, TaskData t ~ d) => t -> m ()
  -- nextScheduledJob :: m (Maybe (Job d))
  -- react            :: (d -> m ()) -> m ()

data Job d where
  Job :: Task t => t -> Job (TaskData t)

instance Task (Job d) where
  type TaskData (Job d) = d

  runAt   (Job task) = runAt task
  nextJob (Job task) = nextJob task
  apply   (Job task) = apply task

newtype InMemorySchedulerState d = InMemorySchedulerState {
  imssJobQueue :: PQ.MinPQueue UTCTime (Job d)
}

newtype Scheduler d m a = Scheduler { unScheduler :: StateT (InMemorySchedulerState d) m a }
                            deriving (
                              Functor, Applicative, Monad,
                              MonadState (InMemorySchedulerState d),
                              MonadThrow, MonadCatch, MonadMask, MonadIO, MonadTrans
                            )

instance forall m d. (Monad m, MonadChronometer m) => MonadScheduler d (Scheduler d m) where
  schedule task = do
    executesAt <- runAt task <$> now

    modify $ \schedulerState@InMemorySchedulerState{..} ->
      schedulerState {
        imssJobQueue = PQ.insert executesAt (Job task) imssJobQueue
      }

  -- nextScheduledJob = do
  --   jobQueue <- gets imssJobQueue

  --   case PQ.minView jobQueue of
  --     Nothing -> return Nothing
  --     Just (job, newQueue) -> do
  --       modify $ \schedulerState -> schedulerState { imssJobQueue = newQueue }
  --       return (Just job)

  -- react handler =
  --   nextScheduledJob >>= \case
  --     Nothing  -> return ()
  --     Just job -> do
  --       wakeupTime  <- now
  --       let runTime = runAt job wakeupTime

  --       sleepUntil runTime

  --       apply job handler

  --       mapM_ schedule (nextJob job runTime)

  --       react handler

runScheduler :: (MonadChronometer m) => Scheduler d m () -> m ()
runScheduler actions = evalStateT (unScheduler actions) (InMemorySchedulerState PQ.empty)

instance (MonadChronometer m, MonadTrans t, Monad (t m)) => MonadChronometer (t m) where
  now = lift now
  sleepUntil = lift . sleepUntil

instance MonadChronometer IO where
  now = getCurrentTime
  sleepUntil wakeupTime = do
    now' <- now

    timerSleep (wakeupTime `diffTime` now')
      where
        timerSleep (Delay interval) = threadDelay (1000 * 1000 * round interval)
-- instance Schedulable (ScheduledJob' jobtype) where
--   type JobData (ScheduledJob' jobtype) = jobtype

--   calculateStartTime Immediately{} = id

--   apply action Immediately{jobdata} = action jobdata

-- data Job = forall sj. (Eq sj, Show sj, Schedulable sj) => Job sj

-- instance Show Job where
--   show (Job j) = show j

-- data JobState =
--   InitialState {
--     jobDefinition :: !Job,
--     referenceTime :: !ReferenceTime
--   } | IntermediateState {
--     jobDefinition :: !Job,
--     lastCompleted :: !UTCTime,
--     lastStatus    :: !Status,
--     lastWakeup    :: !UTCTime,
--     referenceTime :: !ReferenceTime
--   } deriving (Show)

-- data SchedulerState m = SchedulerState {
--   jobQueue            :: !(PQ.MinPQueue UTCTime JobState),
--   stateChangeCallback :: !(Maybe (JobState -> m ()))
-- }

-- newtype Scheduler m a = Scheduler { unScheduler :: StateT (SchedulerState m) m a }
--   deriving (Functor, Applicative, Monad, MonadState (SchedulerState m),
--     MonadThrow, MonadCatch, MonadMask, MonadLogger, MonadIO)

-- instance MonadTrans Scheduler where
--   lift = Scheduler . lift

-- submitJob :: (Timer m) => Job -> Scheduler m ()
-- submitJob (Job job) = do
--   now <- currentTime

--   let startTime = calculateStartTime job now
--       referenceTime = ReferenceTime startTime

--   modify $ \originalState@SchedulerState{jobQueue} ->
--     originalState {
--       jobQueue = PQ.insert startTime (InitialState (Job job) referenceTime) jobQueue
--     }

-- -- removeJob :: (Monad m, Eq jobtype) => ScheduledJobMatcher jobtype -> Scheduler jobtype m ()
-- -- removeJob toRemove = modify $ \SchedulerState{..} ->
-- --   SchedulerState {
-- --     jobQueue = PQ.filter (predicate toRemove) jobQueue,
-- --     ..
-- --   }
-- --   where
-- --     predicate (Exact scheduledJob) JobState{..} = jobDefinition /= scheduledJob
-- --     predicate (ByContent jobData)  JobState{..} = jobdata jobDefinition /= jobData

-- -- clearJobs :: Monad m => Scheduler jobtype m ()
-- -- clearJobs = modify $ \originalState -> originalState { jobQueue = PQ.empty }

-- -- runScheduler :: (Timer m) => [ScheduledJob jobtype] -> Scheduler jobtype m a -> m a
-- -- runScheduler jobs block =

-- --   evalStateT (unScheduler actions) initialState

-- --   where
-- --     actions = mapM_ submitJob jobs >> block

-- --     initialState = SchedulerState {
-- --       jobQueue            = PQ.empty,
-- --       stateChangeCallback = Nothing
-- --     }

-- pullNext :: Timer m => Scheduler m (Maybe JobState)
-- pullNext = undefined
-- -- pullNext = do
-- --   originalState@SchedulerState{..} <- get
-- --   now <- currentTime

-- --   case PQ.getMin jobQueue of
-- --     Nothing -> return Nothing
-- --     Just (wakeupTime, source) -> do
-- --       let remainingJobs = PQ.deleteMin jobQueue

-- --       if now >= wakeupTime
-- --       then do
-- --         put $ originalState { jobQueue = remainingJobs }
-- --         return $ Just source { lastWakeup = Just now }
-- --       else
-- --         return Nothing

-- reschedule :: Timer m => JobState -> Status -> Scheduler m ()
-- reschedule = undefined
-- -- reschedule jobState@JobState{jobDefinition, referenceTime} status' =
-- --   case jobDefinition of
-- --     Immediately{}              -> return ()
-- --     Once{}                     -> return ()
-- --     After{}                    -> return ()
-- --     Periodic{runInterval}      -> reschedulePeriodic runInterval status'
-- --     PeriodicAfter{runInterval} -> reschedulePeriodic runInterval status'
-- --     DailyAt{}                  -> reschedulePeriodic (Interval 86400) status'
-- --   where
-- --     reschedulePeriodic runInterval status = do
-- --       now <- currentTime
-- --       originalState@SchedulerState{stateChangeCallback, jobQueue} <- get

-- --       let newJobState = jobState { lastCompleted = Just now, lastStatus = Just status }
-- --           lastTime = fromMaybe now (lastWakeup jobState)
-- --           nextTime = next referenceTime runInterval lastTime

-- --       put $ originalState { jobQueue = PQ.insert nextTime newJobState jobQueue }

-- --       case stateChangeCallback of
-- --         Nothing     -> return ()
-- --         Just action -> lift $ action newJobState


-- sleep :: Timer m => Scheduler m () -> Scheduler m ()
-- sleep = undefined
-- -- sleep nextAction = do
-- --   SchedulerState{..} <- get

-- --   case PQ.getMin jobQueue of
-- --     Just (wakeupTime, _) -> do
-- --       sleepUntil wakeupTime
-- --       nextAction
-- --     Nothing -> return ()

-- whenReady :: (Timer m) => (forall jobtype. jobtype -> m ExecutionResult) -> Scheduler m ()
-- whenReady action = whenReadyS (lift . action)

-- whenReadyS :: (Timer m) => (forall j. Schedulable j => JobData j -> Scheduler m ExecutionResult) -> Scheduler m ()
-- whenReadyS action =
--   pullNext >>= \case
--     Nothing ->
--       sleep $ whenReadyS action
--     Just readyJobState ->
--         apply' action (jobDefinition readyJobState) >>= \case
--           _ -> undefined

--       -- in
--       --   apply action job >>= \case
--       --     Continue result -> do
--       --       reschedule readyJobState result
--       --       whenReadyS action
--       --     Halt       -> return ()
--   where
--     apply' action' (Job job) = apply action' job
-- -- onStateChange :: (Timer m) => (JobState jobtype -> m ()) -> Scheduler jobtype m ()
-- -- onStateChange action = modify $ \state -> state { stateChangeCallback = Just action }

-- class (Monad m) => Timer m where
--   currentTime :: m UTCTime
--   timerSleep :: Delay -> m ()

-- instance (MonadTrans t, Timer m, (Monad (t m))) => Timer (t m) where
--   currentTime = lift currentTime
--   timerSleep = lift . timerSleep

-- --   sleepUntil :: UTCTime -> m ()
-- --   sleepUntil wakeupTime = do
-- --     now <- currentTime
-- --     timerSleep (wakeupTime `diffTime` now)

-- instance Timer IO where
--   currentTime = getCurrentTime
--   timerSleep (Delay interval) = threadDelay (1000 * 1000 * round interval)
