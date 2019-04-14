{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
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
import           Data.Aeson                    (FromJSON, ToJSON)
import           GHC.Generics                  (Generic)


data Job d = Job {
  jobSchedule :: Schedule,
  jobWorkUnit :: d
} deriving (Show, Generic, ToJSON, FromJSON)

class MonadJobs d m => MonadScheduler d m | m -> d where
  schedule :: Schedule -> d -> m ()
  react    :: (d -> ExecutionMonad m ()) -> m ()

class MonadJobs d m | m -> d where
  type ExecutionMonad m :: * -> *

  pushQueue :: ScheduledTime -> Job d -> m ()
  peekQueue :: m (Maybe (ScheduledTime, Job d))
  dropQueue :: m ()
  execute   :: ExecutionMonad m () -> m ()
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
    mbItem <- peekQueue

    whenJust mbItem $ \(runTime, Job{..}) -> do
      sleepUntil runTime
      now' <- now

      execute (handler jobWorkUnit)

      dropQueue

      whenJust
        (nextJob jobSchedule now')
        (`schedule` jobWorkUnit)

      react handler
