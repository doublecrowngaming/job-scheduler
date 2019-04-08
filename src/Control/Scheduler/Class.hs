{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Scheduler.Class (
  MonadScheduler(..),
  MonadJobs(..),
  Job(..)
) where

import           Control.Scheduler.Chronometer (MonadChronometer (..))
import           Control.Scheduler.Schedule    (Schedule (..), nextJob, runAt)
import           Control.Scheduler.Time        (ScheduledTime (..))

data Job d = Job {
  jobSchedule :: Schedule,
  jobWorkUnit :: d
} deriving Show

class (MonadChronometer m, MonadJobs d m) => MonadScheduler d m | m -> d where
  schedule :: Schedule -> d -> m ()
  react    :: (d -> m ()) -> m ()

class MonadJobs d m | m -> d where
  pushQueue :: ScheduledTime -> Job d -> m ()
  popQueue  :: m (Maybe (ScheduledTime, Job d))
  execute   :: m () -> m ()
  enumerate :: m [(ScheduledTime, Job d)]

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust Nothing  _      = pure ()
whenJust (Just x) action = action x

instance (Monad m, MonadChronometer m, MonadJobs d m) => MonadScheduler d m where
  schedule task datum = do
    mbExecutesAt <- runAt task <$> now

    whenJust mbExecutesAt $ \executesAt ->
      pushQueue executesAt (Job task datum)

  react handler = do
    mbItem <- popQueue

    whenJust mbItem $ \(runTime, Job{..}) -> do
        sleepUntil runTime

        execute (handler jobWorkUnit)

        mbNextJob <- nextJob jobSchedule <$> now

        whenJust mbNextJob (`schedule` jobWorkUnit)

        react handler
