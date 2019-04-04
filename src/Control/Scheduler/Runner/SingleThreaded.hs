{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

module Control.Scheduler.Runner.SingleThreaded (
  SingleThreaded,
  setSchedulerEndTime
) where

import           Control.Monad                (when)
import           Control.Monad.State.Strict   (evalStateT, gets, modify)
import           Control.Scheduler.Class      (MonadJobs (..))
import           Control.Scheduler.Task.Class (Job (..))
import           Control.Scheduler.Time       (ScheduledTime (..))
import           Control.Scheduler.Type       (RunnableScheduler (..),
                                               Scheduler, unScheduler)
import qualified Data.PQueue.Prio.Min         as PQ


data SingleThreaded d = SingleThreaded {
  stJobQueue :: PQ.MinPQueue ScheduledTime (Job d),
  stEndTime  :: Maybe ScheduledTime
}

setSchedulerEndTime :: Monad m => ScheduledTime -> Scheduler SingleThreaded d m ()
setSchedulerEndTime endTime = modify $ \schedulerState@SingleThreaded{..} ->
                                          schedulerState { stEndTime = Just endTime }

instance Monad m => MonadJobs d (Scheduler SingleThreaded d m) where
  pushQueue executesAt item = do
    mbEndTime <- gets stEndTime

    case mbEndTime of
      Nothing      -> insertJob
      Just endTime -> when (executesAt <= endTime) insertJob

    where
      insertJob = modify $ \schedulerState@SingleThreaded{..} ->
                              schedulerState {
                                stJobQueue = PQ.insert executesAt item stJobQueue
                              }

  popQueue = do
    jobQueue <- gets stJobQueue

    case PQ.minViewWithKey jobQueue of
      Nothing -> return Nothing
      Just ((time, job), newQueue) -> do
        modify $ \schedulerState -> schedulerState { stJobQueue = newQueue }
        return $ Just (time, job)

  execute = id

instance RunnableScheduler SingleThreaded where
  runScheduler actions = evalStateT (unScheduler actions) (SingleThreaded PQ.empty Nothing)
