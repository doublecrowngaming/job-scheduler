{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Scheduler.Schedule.At (
  At(..)
) where

import           Control.Scheduler.Schedule.Class (Schedule (..))
import           Control.Scheduler.Time           (ScheduledTime (..))
import           Data.Aeson                       (FromJSON (..), ToJSON (..))

newtype At = At ScheduledTime deriving (Eq, Show, ToJSON, FromJSON)

instance Schedule At where
  runAt   (At scheduledTime) = const (Just scheduledTime)
  nextJob (At _)             = const Nothing
