{-# LANGUAGE TypeFamilies #-}

module Control.Scheduler.Task.Cron (
  Cron(..)
) where

import           Control.Scheduler.Task.Class (Job (..), Task (..))
import           Control.Scheduler.Time       (CurrentTime (..),
                                               ScheduledTime (..))
import           System.Cron                  (CronSchedule, nextMatch)

data Cron d = Cron CronSchedule d deriving (Eq, Show)

instance Task (Cron d) where
  type TaskData (Cron d) = d

  runAt (Cron sched _) (CurrentTime now) = ScheduledTime <$> nextMatch sched now
  nextJob                                = const . Just .  Job
  apply (Cron _ d) f                     = f d
