{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Control.Scheduler.Enrichments.Prometheus (
  Prometheus,
  withPrometheus
) where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Strict (gets)
import           Control.Monad.Trans.Class  (lift)
import           Control.Scheduler.Class    (MonadJobs (..))
import           Control.Scheduler.Type     (Enrichment (..), Iso (..),
                                             Scheduler, stack, unstackM)
import           Prometheus


data Prometheus r d = Prometheus {
  unPrometheus  :: r d,
  pQueueDepth   :: Gauge,
  pJobsAdded    :: Counter,
  pJobsPeeked   :: Counter,
  pJobsDropped  :: Counter,
  pJobsExecuted :: Counter
}

instance Enrichment (Prometheus r d) (r d) where
  enrich oldPrometheus = Iso (\x -> oldPrometheus { unPrometheus = x }) unPrometheus

initializeMetrics :: (MonadIO m, MonadJobs d (Scheduler r d m)) => r d -> m (Prometheus r d)
initializeMetrics base =
  Prometheus
    <$> pure base
    <*> register queueDepthMetric
    <*> register jobsAddedMetric
    <*> register jobsPeekedMetric
    <*> register jobsDroppedMetric
    <*> register jobsExecutedMetric

  where
    queueDepthMetric   = gauge   (Info "scheduler_queue_depth"   "Scheduler work queue depth")
    jobsAddedMetric    = counter (Info "scheduler_jobs_added"    "Number of jobs added to work queue")
    jobsPeekedMetric   = counter (Info "scheduler_jobs_peeked"   "Number of times the head of the queue has been looked at")
    jobsDroppedMetric  = counter (Info "scheduler_jobs_deleted"  "Number of jobs removed from the work queue")
    jobsExecutedMetric = counter (Info "scheduler_jobs_executed" "Number of jobs executed")

trackQueueDepth :: (MonadMonitor m, MonadJobs d (Scheduler r d m)) => Scheduler (Prometheus r) d m ()
trackQueueDepth = do
  depthGauge <- gets pQueueDepth
  setGauge depthGauge . fromIntegral . length =<< enumerate

instance MonadMonitor m => MonadMonitor (Scheduler (Prometheus r) d m) where
  doIO = lift . doIO

instance (Monad m, MonadMonitor m, MonadJobs d (Scheduler r d m)) => MonadJobs d (Scheduler (Prometheus r) d m) where
  type ExecutionMonad (Scheduler (Prometheus r) d m) = ExecutionMonad (Scheduler r d m)

  pushQueue executesAt item = do
    stack $ pushQueue executesAt item

    trackQueueDepth
    incCounter =<< gets pJobsAdded

  peekQueue = do
    item <- stack peekQueue
    incCounter =<< gets pJobsPeeked
    return item

  dropQueue = do
    stack dropQueue
    trackQueueDepth
    incCounter =<< gets pJobsDropped

  execute action = do
    stack $ execute action
    trackQueueDepth
    incCounter =<< gets pJobsExecuted

  enumerate = stack enumerate

withPrometheus :: (MonadIO io, MonadJobs d (Scheduler r d io)) => Scheduler (Prometheus r) d io () -> Scheduler r d io ()
withPrometheus = unstackM initializeMetrics
