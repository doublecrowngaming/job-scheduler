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
import           Control.Scheduler.Class    (MonadJobs (..))
import           Control.Scheduler.Type     (Enrichment (..), Iso (..),
                                             Scheduler, stack, unstackM)
import           Prometheus


data Prometheus r d = Prometheus {
  unPrometheus :: r d,
  pGauges      :: Vector Label1 Gauge,
  pCounters    :: Vector Label1 Counter
}

instance Enrichment (Prometheus r d) (r d) where
  enrich oldPrometheus = Iso (\x -> oldPrometheus { unPrometheus = x }) unPrometheus

initializeMetrics :: (MonadIO m, MonadJobs d (Scheduler r d m)) => r d -> m (Prometheus r d)
initializeMetrics base =
  Prometheus
    <$> pure base
    <*> register gauges
    <*> register counters

  where
    counters           = vector "counter" $ counter (Info "scheduler_operations" "Scheduler operation counters")
    gauges             = vector "gauge"   $ gauge   (Info "scheduler" "Scheduler gauges")

trackQueueDepth :: (MonadIO m, MonadJobs d (Scheduler r d m)) => Scheduler (Prometheus r) d m ()
trackQueueDepth = do
  gauges <- gets pGauges
  depth  <- fromIntegral . length <$> enumerate

  withLabel gauges "queue_depth" (`setGauge` depth)

incCounterL :: (Label l, MonadMonitor m) => l -> Vector l Counter -> m ()
incCounterL label vec = withLabel vec label incCounter

instance MonadIO io => MonadMonitor (Scheduler (Prometheus r) d io) where
  doIO = liftIO . doIO

instance (Monad m, MonadIO m, MonadJobs d (Scheduler r d m)) => MonadJobs d (Scheduler (Prometheus r) d m) where
  type ExecutionMonad (Scheduler (Prometheus r) d m) = ExecutionMonad (Scheduler r d m)

  pushQueue executesAt item = do
    stack $ pushQueue executesAt item

    trackQueueDepth
    incCounterL "jobs_added" =<< gets pCounters

  peekQueue = do
    item <- stack peekQueue
    incCounterL "jobs_peeked" =<< gets pCounters
    return item

  dropQueue = do
    stack dropQueue
    trackQueueDepth
    incCounterL "jobs_deleted" =<< gets pCounters

  execute action = do
    stack $ execute action
    trackQueueDepth
    incCounterL "jobs_executed" =<< gets pCounters

  enumerate = stack enumerate

withPrometheus :: (MonadIO io, MonadJobs d (Scheduler r d io)) => Scheduler (Prometheus r) d io () -> Scheduler r d io ()
withPrometheus = unstackM initializeMetrics
