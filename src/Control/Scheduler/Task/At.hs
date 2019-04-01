{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Scheduler.Task.At (
  At(..)
) where

import           Control.Scheduler.Task.Class (Task (..))
import           Control.Scheduler.Time       (ScheduledTime (..))

data At d = At ScheduledTime d deriving (Eq, Show)

instance Task (At d) where
  type TaskData (At d) = d

  runAt   (At scheduledTime _) = const (Just scheduledTime)
  nextJob (At _ _)             = const Nothing
  apply   (At _ d) f           = f d
