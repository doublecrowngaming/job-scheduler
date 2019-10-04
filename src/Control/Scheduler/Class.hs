{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Scheduler.Class (
  MonadScheduler(..),
  MonadJobs(..),
  Job(..)
) where

import           Control.Monad                 (when)
import           Control.Scheduler.Chronometer (MonadChronometer (..),
                                                TimerResult (..))
import           Control.Scheduler.Schedule    (Schedule (..), nextJob, runAt)
import           Control.Scheduler.Time        (ScheduledTime (..), isAtOrAfter)
import           Data.Aeson                    (FromJSON, ToJSON)
import           GHC.Generics                  (Generic)

data Job d = Job {
  jobSchedule :: Schedule,
  jobWorkUnit :: d
} deriving (Show, Generic, ToJSON, FromJSON)

class MonadJobs d m => MonadScheduler d m | m -> d where
  schedule :: Job d -> m ()
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

instance (Monad m, MonadChronometer m (Job d), MonadJobs d m) => MonadScheduler d m where
  schedule job = do
    mbExecutesAt <- jobRunAt job <$> now

    whenJust mbExecutesAt $ \executesAt ->
      pushQueue executesAt job
    where
      jobRunAt Job{..} = runAt jobSchedule

  react handler = do
    mbItem <- peekQueue

    whenJust mbItem $ \(runTime, job) ->
      at runTime $ \case
        Expiration -> do
          now' <- now

          when (now' `isAtOrAfter` runTime) $
            runJob now' job

          react handler

        Interrupt job -> do
          schedule job
          react handler

    where
      runJob now' Job{..} = do
        execute (handler jobWorkUnit)

        dropQueue

        whenJust (nextJob jobSchedule now') $ \jobSchedule' ->
          schedule (Job jobSchedule' jobWorkUnit)
