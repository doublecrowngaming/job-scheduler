{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Scheduler.Task.Immediately (
  Immediately(..)
) where

import           Control.Scheduler.Task.Class (Task (..))
import           Control.Scheduler.Time       (CurrentTime (..),
                                               ScheduledTime (..))

newtype Immediately d = Immediately d deriving (Eq, Show)

instance Task (Immediately d) where
  type TaskData (Immediately d) = d

  runAt   (Immediately _) (CurrentTime now) = Just $ ScheduledTime now
  nextJob (Immediately _)                   = const Nothing
  apply   (Immediately d) f                 = f d
