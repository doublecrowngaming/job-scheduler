{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Scheduler.Enrichments.Checkpointing (
  Checkpointing,
  withCheckpointing,
  onColdStart
) where

import           Control.Monad              (when)
import           Control.Monad.Catch        (MonadCatch, handleIf)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Strict (gets)
import           Control.Scheduler.Class    (Job, MonadJobs (..))
import           Control.Scheduler.Time     (ScheduledTime)
import           Control.Scheduler.Type     (Enrichment (..), Iso (..),
                                             Scheduler, stack, unstack)
import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             eitherDecodeFileStrict',
                                             encodeFile)
import           System.IO.Error            (isDoesNotExistError)


data Checkpointing r d = Checkpointing {
  unCheckpointing :: r d,
  dcFilename      :: FilePath
}

writeCheckpointFile :: (MonadIO io, ToJSON d, MonadJobs d (Scheduler r d io)) => Scheduler (Checkpointing r) d io ()
writeCheckpointFile = do
  enumeration <- enumerate
  filename    <- gets dcFilename

  liftIO $ encodeFile filename enumeration

readCheckpointFile :: forall io d r. (MonadIO io, FromJSON d, ToJSON d, MonadJobs d (Scheduler r d io), MonadCatch io) => Scheduler (Checkpointing r) d io ()
readCheckpointFile = do
  filename <- gets dcFilename

  handleIf isDoesNotExistError
    (const $ return ())
    (do
      result   <- liftIO $ eitherDecodeFileStrict' filename

      case result of
        Left err  -> error err
        Right val -> mapM_ (uncurry pushQueue) (val :: [(ScheduledTime, Job d)])
    )

onColdStart :: (MonadIO io, ToJSON d, MonadJobs d (Scheduler r d io)) => Scheduler (Checkpointing r) d io () -> Scheduler (Checkpointing r) d io ()
onColdStart actions = do
  enumeration <- enumerate

  when (null enumeration)
    actions

instance Enrichment (Checkpointing r d) (r d) where
  enrich Checkpointing{dcFilename} = Iso (`Checkpointing` dcFilename) unCheckpointing

instance (MonadIO io, ToJSON d, MonadJobs d (Scheduler r d io)) => MonadJobs d (Scheduler (Checkpointing r) d io) where
  type ExecutionMonad (Scheduler (Checkpointing r) d io) = ExecutionMonad (Scheduler r d io)

  pushQueue executesAt item = do
    stack $ pushQueue executesAt item
    writeCheckpointFile

  peekQueue = stack peekQueue

  dropQueue = do
    stack dropQueue
    writeCheckpointFile

  execute action = do
    stack $ execute action
    writeCheckpointFile

  enumerate = stack enumerate

withCheckpointing :: (ToJSON d, FromJSON d, MonadIO io, MonadJobs d (Scheduler r d io), MonadCatch io) => FilePath -> Scheduler (Checkpointing r) d io () -> Scheduler r d io ()
withCheckpointing filename actions =
  unstack (`Checkpointing` filename) $ do
    readCheckpointFile
    actions
