{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Scheduler.Schedule (
  module Control.Scheduler.Schedule,
  module Control.Scheduler.Schedule.Class,
  module Control.Scheduler.Schedule.After,
  module Control.Scheduler.Schedule.At,
  module Control.Scheduler.Schedule.Cron,
  module Control.Scheduler.Schedule.Every,
  module Control.Scheduler.Schedule.Immediately
) where

import           Control.Scheduler.Schedule.After
import           Control.Scheduler.Schedule.At
import           Control.Scheduler.Schedule.Class
import           Control.Scheduler.Schedule.Cron
import           Control.Scheduler.Schedule.Every
import           Control.Scheduler.Schedule.Immediately

import           Data.Aeson                             (FromJSON (..),
                                                         ToJSON (..), object,
                                                         (.=))

data Job where
  Job :: Schedule t => t -> Job

deriving instance Show Job

instance ToJSON Job where
  toJSON = undefined

instance FromJSON Job where
  parseJSON = undefined

instance Schedule Job where
  runAt   (Job schedule)      = runAt schedule
  nextJob (Job schedule) time = Job <$> nextJob schedule time
