{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Scheduler.Task.Every (
  Every(Every)
) where

import           Control.Scheduler.Task.Class (Job (..), Task (..))
import           Control.Scheduler.Time       (Delay, Interval (..), addTime)

data Every d =
    Every  Delay Interval d
  | Every'       Interval d
  deriving (Eq, Show)

instance Task (Every d) where
  type TaskData (Every d) = d

  runAt   (Every  delay _        _) = Just . (`addTime` delay)
  runAt   (Every'       interval _) = Just . (`addTime` interval)

  nextJob (Every  _    interval d) = const . Just . Job $ Every' interval d
  nextJob (Every'      interval d) = const . Just . Job $ Every' interval d

  apply   (Every  _     _       d) f = f d
  apply   (Every'       _       d) f = f d
