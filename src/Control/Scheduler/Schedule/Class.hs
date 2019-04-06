module Control.Scheduler.Schedule.Class (
  Schedule(..)
) where

import           Control.Scheduler.Time (CurrentTime, ScheduledTime)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Functor.Identity  (Identity (..))
import           Data.Typeable

class (Show t, ToJSON t, FromJSON t) => Schedule t where
  runAt   :: t -> CurrentTime -> Maybe ScheduledTime
  nextJob :: t -> CurrentTime -> Maybe t
