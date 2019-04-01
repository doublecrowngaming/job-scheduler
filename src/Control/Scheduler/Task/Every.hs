{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Scheduler.Task.Every (
  Every(..)
) where

import           Control.Scheduler.Task.Class (Job (..), Task (..))
import           Control.Scheduler.Time       (Delay, Interval (..), addTime)

data Every d = Every Delay Interval d deriving (Eq, Show)

instance Task (Every d) where
  type TaskData (Every d) = d

  runAt   (Every delay interval _)   = Just . (`addTime` delay)
  nextJob                            = const . Just . Job
  apply   (Every _     _        d) f = f d
