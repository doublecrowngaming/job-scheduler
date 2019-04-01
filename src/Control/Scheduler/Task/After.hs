{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Scheduler.Task.After (
  After(..)
) where

import           Control.Scheduler.Task.Class (Task (..))
import           Control.Scheduler.Time       (Delay (..), addTime)

data After d = After Delay d deriving (Eq, Show)

instance Task (After d) where
  type TaskData (After d) = d

  runAt   (After delay _) = Just . (`addTime` delay)
  nextJob (After _ _)     = const Nothing
  apply   (After _ d) f   = f d
