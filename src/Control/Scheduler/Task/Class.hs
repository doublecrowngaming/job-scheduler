{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Scheduler.Task.Class (
  Task(..),
  Job(..)
) where

import           Control.Scheduler.Time (CurrentTime, ScheduledTime)

class Task t where
  type TaskData t :: *

  runAt   :: t -> CurrentTime -> ScheduledTime
  nextJob :: t -> CurrentTime -> Maybe (Job (TaskData t))
  apply   :: t -> (TaskData t -> m a) -> m a

data Job d where
  Job :: Task t => t -> Job (TaskData t)


instance Task (Job d) where
  type TaskData (Job d) = d

  runAt   (Job task) = runAt task
  nextJob (Job task) = nextJob task
  apply   (Job task) = apply task
