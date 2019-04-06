{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Scheduler.Enrichments.Checkpointing (
  Checkpointing,
  withCheckpointing
) where


import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Strict (gets, modify)
import           Control.Scheduler.Class    (MonadJobs (..))
import           Control.Scheduler.Schedule (Job)
import           Control.Scheduler.Type     (Enrichment (..), Iso (..),
                                             RunnableScheduler (..), Scheduler,
                                             embed, stack, unstack)
import           Data.Aeson                 (FromJSON (..), ToJSON (..), Value,
                                             eitherDecode, encodeFile)
import           Data.Bifunctor             (first)

newtype SerializationError = SerializationError String

data Checkpointing r d = Checkpointing {
  unCheckpointing :: r d,
  dcFilename      :: FilePath
}

writeCheckpointFile :: (MonadIO io, ToJSON d, MonadJobs d (Scheduler r d io)) => Scheduler (Checkpointing r) d io ()
writeCheckpointFile = do
  enumeration <- enumerate
  filename    <- gets dcFilename

  liftIO $ encodeFile filename enumeration

instance Enrichment (Checkpointing r d) (r d) where
  enrich (Checkpointing _ dcFilename) = Iso (`Checkpointing` dcFilename) unCheckpointing
  strip                               = Iso unCheckpointing (`Checkpointing` undefined)

instance (MonadIO io, ToJSON d, MonadJobs d (Scheduler r d io)) => MonadJobs d (Scheduler (Checkpointing r) d io) where
  pushQueue executesAt item = do
    stack $ pushQueue executesAt item
    writeCheckpointFile

  popQueue = stack popQueue

  execute action =
    stack . execute $ unstack action

withCheckpointing :: (RunnableScheduler r, MonadIO io) => FilePath -> Scheduler (Checkpointing r) d io () -> Scheduler r d io ()
withCheckpointing filename = embed (`Checkpointing` filename)
