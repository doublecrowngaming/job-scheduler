{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Control.Scheduler.Schedule.Immediately (
  Immediately(..)
) where

import           Control.Scheduler.Schedule.Class (Schedule (..))
import           Control.Scheduler.Time           (CurrentTime (..),
                                                   ScheduledTime (..))
import           Data.Aeson                       (FromJSON (..), ToJSON (..))
import           GHC.Generics                     (Generic)

data Immediately = Immediately deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Schedule Immediately where
  runAt   Immediately (CurrentTime now) = Just $ ScheduledTime now
  nextJob Immediately                   = const Nothing
