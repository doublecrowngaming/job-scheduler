{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Scheduler.Checkpointer.Disk (
  DiskCheckpointing,
  setCheckpointFilename,
  withDiskCheckpointing
) where


import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Strict (gets, modify)
import           Control.Scheduler.Class    (MonadJobs (..))
import           Control.Scheduler.Type     (Enrichment (..), Iso (..),
                                             RunnableScheduler (..), Scheduler,
                                             embed, stack, unstack)
import           Debug.Trace

setCheckpointFilename :: MonadIO io => FilePath -> Scheduler (DiskCheckpointing r) d io ()
setCheckpointFilename filename =
  modify $ \oldState@DiskCheckpointing{..} ->
    oldState { dcFilename = filename }

data DiskCheckpointing r d = DiskCheckpointing {
  unDiskCheckpointing :: r d,
  dcFilename          :: FilePath
}

instance Enrichment (DiskCheckpointing r d) (r d) where
  enrich (DiskCheckpointing _ dcFilename) = Iso (`DiskCheckpointing` dcFilename) unDiskCheckpointing
  strip                                   = Iso unDiskCheckpointing (`DiskCheckpointing` undefined)

instance (Monad m, MonadJobs d (Scheduler r d m)) => MonadJobs d (Scheduler (DiskCheckpointing r) d m) where
  pushQueue executesAt item = do
    stack $ pushQueue executesAt item
    gets dcFilename >>= traceM

  popQueue = do
    gets dcFilename >>= traceM
    stack popQueue

  execute action = do
    stack . execute $ unstack action
    gets dcFilename >>= traceM

withDiskCheckpointing :: (RunnableScheduler r, MonadIO io) => FilePath -> Scheduler (DiskCheckpointing r) d io () -> Scheduler r d io ()
withDiskCheckpointing filename = embed (`DiskCheckpointing` filename)
