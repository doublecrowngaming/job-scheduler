{-# OPTIONS_HADDOCK hide #-}

module Control.Scheduler.Schedule (
  Schedule(After, At, Cron, Every, Immediately),
  runAt,
  nextJob
) where

import           Control.Scheduler.Time (CurrentTime (..), Delay (..),
                                         Interval (..), ScheduledTime (..),
                                         addTime)
import           System.Cron            (CronSchedule, nextMatch)

data Schedule =
    After Delay
  | At    ScheduledTime
  | Cron  CronSchedule
  | Every Delay Interval
  | Every'      Interval
  | Immediately
  deriving (Show, Eq)

runAt :: Schedule -> CurrentTime -> Maybe ScheduledTime
runAt (After delay)              now = Just $ now `addTime` delay
runAt (At scheduledTime)         _   = Just scheduledTime
runAt (Cron sched) (CurrentTime now) = ScheduledTime <$> nextMatch sched now
runAt (Every  delay _       )    now = Just $ now `addTime` delay
runAt (Every'       interval)    now = Just $ now `addTime` interval
runAt Immediately (CurrentTime now)  = Just $ ScheduledTime now

nextJob :: Schedule -> CurrentTime -> Maybe Schedule
nextJob (After _)           _ = Nothing
nextJob Immediately         _ = Nothing
nextJob (Every  _ interval) _ = Just $ Every' interval
nextJob (Every'   interval) _ = Just $ Every' interval
nextJob (Cron s)            _ = Just $ Cron s
nextJob (At _)              _ = Nothing
