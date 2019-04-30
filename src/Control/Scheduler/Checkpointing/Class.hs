{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans       #-}

module Control.Scheduler.Checkpointing.Class (
  Checkpointer(..),
  Checkpointing,
  onColdStart,
  runCheckpointer
) where

import           Control.Monad              (when)
import           Control.Monad.Catch        (MonadCatch)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.State.Strict (gets)
import           Control.Scheduler.Class    (Job, MonadJobs (..))
import           Control.Scheduler.Time     (ScheduledTime)
import           Control.Scheduler.Type     (Enrichment (..), Iso (..),
                                             Scheduler, stack, unstack)
import           Data.Aeson                 (FromJSON, ToJSON)


data Checkpointer io r d = Checkpointer {
  writeCheckpoint :: Scheduler r d io (),
  readCheckpoint  ::  Scheduler r d io [(ScheduledTime, Job d)]
}

data Checkpointing m r d = Checkpointing {
  unCheckpointing :: r d,
  checkpointer    :: Checkpointer m r d
}

instance Enrichment (Checkpointing m r d) (r d) where
  enrich oldCheckpointing = Iso (\x -> oldCheckpointing { unCheckpointing = x }) unCheckpointing

instance (MonadIO m, MonadCatch m, ToJSON d, MonadJobs d (Scheduler r d m)) => MonadJobs d (Scheduler (Checkpointing m r) d m) where
  type ExecutionMonad (Scheduler (Checkpointing m r) d m) = ExecutionMonad (Scheduler r d m)

  pushQueue executesAt item = do
    Checkpointer{..} <- gets checkpointer

    stack $ do
      pushQueue executesAt item
      writeCheckpoint

  peekQueue = stack peekQueue

  dropQueue = do
    Checkpointer{..} <- gets checkpointer

    stack $ do
      dropQueue
      writeCheckpoint

  execute action = do
    Checkpointer{..} <- gets checkpointer

    stack $ do
      execute action
      writeCheckpoint

  enumerate = stack enumerate

onColdStart :: (MonadIO m, MonadCatch m, ToJSON d, MonadJobs d (Scheduler r d m)) => Scheduler (Checkpointing m r) d m () -> Scheduler (Checkpointing m r) d m ()
onColdStart actions = do
  enumeration <- enumerate

  when (null enumeration)
    actions


runCheckpointer :: (MonadIO m, MonadCatch m, FromJSON d, MonadJobs d (Scheduler r d m)) => Checkpointer m r d -> Scheduler (Checkpointing m r) d m () -> Scheduler r d m ()
runCheckpointer checkpointer actions = do
  persistedJobs <- readCheckpoint checkpointer

  unstack (`Checkpointing` checkpointer) $ do
    stack $ mapM_ (uncurry pushQueue) persistedJobs
    actions
