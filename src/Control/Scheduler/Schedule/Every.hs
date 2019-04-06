{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Control.Scheduler.Schedule.Every (
  Every(Every)
) where

import           Control.Scheduler.Schedule.Class (Schedule (..))
import           Control.Scheduler.Time           (Delay, Interval (..),
                                                   addTime)
import           Data.Aeson                       (FromJSON (..), ToJSON (..))
import           GHC.Generics                     (Generic)

data Every =
    Every  Delay Interval
  | Every'       Interval
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Schedule Every where
  runAt   (Every  delay _       ) = Just . (`addTime` delay)
  runAt   (Every'       interval) = Just . (`addTime` interval)

  nextJob (Every  _    interval) = const . Just $ Every' interval
  nextJob (Every'      interval) = const . Just $ Every' interval
