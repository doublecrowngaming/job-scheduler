{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Scheduler.Schedule.After (
  After(..)
) where

import           Control.Scheduler.Schedule.Class (Schedule (..))
import           Control.Scheduler.Time           (Delay (..), addTime)
import           Data.Aeson                       (FromJSON (..), ToJSON (..))

newtype After = After Delay deriving (Eq, Show, ToJSON, FromJSON)

instance Schedule After where
  runAt   (After delay) = Just . (`addTime` delay)
  nextJob (After _)     = const Nothing
