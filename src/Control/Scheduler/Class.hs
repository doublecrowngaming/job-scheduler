{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Scheduler.Class (
  MonadScheduler(..),
  MonadJobs(..)
) where

import           Control.Scheduler.Chronometer (MonadChronometer (..))
import           Control.Scheduler.Task.Class  (Job (..), Task (..))
import           Control.Scheduler.Time        (ScheduledTime (..))

class (MonadChronometer m, MonadJobs d m) => MonadScheduler d m | m -> d where
  schedule :: (Task t, TaskData t ~ d) => t -> m ()
  react    :: (d -> m ()) -> m ()

class MonadJobs d m | m -> d where
  pushQueue :: ScheduledTime -> Job d -> m ()
  popQueue  :: m (Maybe (Job d))
  execute   :: m () -> m ()


instance (Monad m, MonadChronometer m, MonadJobs d m) => MonadScheduler d m where
  schedule task = do
    executesAt <- runAt task <$> now
    pushQueue executesAt (Job task)

  react handler =
    popQueue >>= \case
      Nothing  -> return ()
      Just job -> do
        runTime <- runAt job <$> now

        sleepUntil runTime

        execute (apply job handler)

        mbNextJob <-  nextJob job <$> now

        mapM_ schedule mbNextJob

        react handler
