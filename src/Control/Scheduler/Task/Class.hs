{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Scheduler.Task.Class (
  Task(..),
  Job(..)
) where

import           Control.Scheduler.Time (CurrentTime, ScheduledTime)
import           Data.Functor.Identity  (Identity (..))

class Task t where
  type TaskData t :: *

  runAt   :: t -> CurrentTime -> Maybe ScheduledTime
  nextJob :: t -> CurrentTime -> Maybe (Job (TaskData t))
  apply   :: Functor f => t -> (TaskData t -> f a) -> f a

data Job d where
  Job :: Task t => t -> Job (TaskData t)

instance Show d => Show (Job d) where
  show (Job d) = "Job " <> runIdentity (apply d (pure . show)) <> ""

instance Task (Job d) where
  type TaskData (Job d) = d

  runAt   (Job task) = runAt task
  nextJob (Job task) = nextJob task
  apply   (Job task) = apply task
