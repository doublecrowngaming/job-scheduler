{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Scheduler.Checkpointer.Disk (
  DiskCheckpointing
) where

import           Control.Monad                (when)
import           Control.Monad.State.Strict   (MonadState (..), evalStateT,
                                               gets, modify)
import           Control.Scheduler.Class      (MonadJobs (..))
import           Control.Scheduler.Task.Class (Job (..))
import           Control.Scheduler.Time       (ScheduledTime (..))
import           Control.Scheduler.Type       (DiskCheckpointing (..),
                                               RunnableScheduler (..),
                                               Scheduler, stack, unScheduler,
                                               unstack)

import           Debug.Trace
-- newtype DiskCheckpointing r d = DiskCheckpointing { unDiskCheckpointing :: r d }

writeCheckpoint :: String -> Scheduler (DiskCheckpointing r) d m ()
writeCheckpoint = undefined

instance (Monad m, MonadJobs d (Scheduler r d m)) => MonadJobs d (Scheduler (DiskCheckpointing r) d m) where
  pushQueue executesAt item = do
    stack $ pushQueue executesAt item
    traceM "pushQueue"

  popQueue = do
    traceM "popQueue"
    stack popQueue

  execute action = do
    stack . execute $ unstack action
    traceM "execute"

instance RunnableScheduler r => RunnableScheduler (DiskCheckpointing r) where
  runScheduler = runScheduler . unstack
