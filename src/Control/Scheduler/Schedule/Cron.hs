module Control.Scheduler.Schedule.Cron (
  Cron(..)
) where

import           Control.Scheduler.Schedule.Class (Schedule (..))
import           Control.Scheduler.Time           (CurrentTime (..),
                                                   ScheduledTime (..))
import           Data.Aeson                       (FromJSON (..), ToJSON (..),
                                                   withText)
import           System.Cron                      (CronSchedule, nextMatch,
                                                   parseCronSchedule,
                                                   serializeCronSchedule)

newtype Cron = Cron CronSchedule deriving (Eq, Show)

instance Schedule Cron where
  runAt (Cron sched) (CurrentTime now) = ScheduledTime <$> nextMatch sched now
  nextJob                              = const . Just

instance ToJSON Cron where
  toJSON (Cron sched) = toJSON $ serializeCronSchedule sched

instance FromJSON Cron where
  parseJSON = withText "Cron expression" $ \text ->
                case parseCronSchedule text of
                  Left err  -> fail err
                  Right val -> return (Cron val)
