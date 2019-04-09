{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Control.Scheduler.Schedule (
  Schedule(After, At, Cron, Every, Immediately),
  runAt,
  nextJob
) where

import           Control.Scheduler.Time (CurrentTime (..), Delay (..),
                                         Interval (..), ScheduledTime (..),
                                         addTime)
import           Data.Aeson             (FromJSON (..), ToJSON (..), withText)
import           GHC.Generics           (Generic)
import           System.Cron            (CronSchedule, nextMatch,
                                         parseCronSchedule,
                                         serializeCronSchedule)


instance ToJSON CronSchedule where
  toJSON sched = toJSON $ serializeCronSchedule sched

instance FromJSON CronSchedule where
  parseJSON = withText "Cron expression" $ \text ->
                case parseCronSchedule text of
                  Left err  -> fail err
                  Right val -> return val


data Schedule =
    After Delay
  | At    ScheduledTime
  | Cron  CronSchedule
  | Every Delay Interval
  | Every'      Interval
  | Immediately
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

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
