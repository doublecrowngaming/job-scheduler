{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

module Control.Scheduler.Runner.SingleThreaded (
  SingleThreaded
) where


import           Control.Monad.State.Strict   (evalStateT, gets, modify)
import           Control.Scheduler.Class      (MonadJobs (..))
import           Control.Scheduler.Task.Class (Job (..))
import           Control.Scheduler.Time       (ScheduledTime (..))
import           Control.Scheduler.Type       (RunnableScheduler (..),
                                               Scheduler, unScheduler)
import qualified Data.PQueue.Prio.Min         as PQ


newtype SingleThreaded d = SingleThreaded {
  stJobQueue :: PQ.MinPQueue ScheduledTime (Job d)
}

instance Monad m => MonadJobs d (Scheduler SingleThreaded d m) where
  pushQueue executesAt item =
    modify $ \schedulerState@SingleThreaded{..} ->
      schedulerState {
        stJobQueue = PQ.insert executesAt item stJobQueue
      }

  popQueue = do
    jobQueue <- gets stJobQueue

    case PQ.minView jobQueue of
      Nothing -> return Nothing
      Just (job, newQueue) -> do
        modify $ \schedulerState -> schedulerState { stJobQueue = newQueue }
        return (Just job)

  execute = id

instance RunnableScheduler SingleThreaded where
  runScheduler actions = evalStateT (unScheduler actions) (SingleThreaded PQ.empty)
