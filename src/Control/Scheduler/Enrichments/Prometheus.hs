{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Control.Scheduler.Enrichments.Prometheus (
  Prometheus,
  withPrometheus,
  PrometheusJob(..)
) where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Strict (gets)
import           Control.Scheduler.Class    (MonadJobs (..))
import           Control.Scheduler.Time     (ScheduledTime (..))
import           Control.Scheduler.Type     (Enrichment (..), Iso (..),
                                             Scheduler, stack, unstackM)
import           Data.Proxy
import           Data.Time.Clock            (NominalDiffTime, diffUTCTime,
                                             getCurrentTime)
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)
import           Prometheus

class (Label (PrometheusJobLabel d)) => PrometheusJob d where
  type PrometheusJobLabel d :: *

  jobLabelFields :: Proxy d -> PrometheusJobLabel d
  jobLabel       :: d -> PrometheusJobLabel d

data Prometheus r d = Prometheus {
  unPrometheus :: r d,
  pGauges      :: Vector Label1 Gauge,
  pCounters    :: Vector Label1 Counter,
  pSummaries   :: Vector (PrometheusJobLabel d) Summary
}

instance Enrichment (Prometheus r d) (r d) where
  enrich oldPrometheus = Iso (\x -> oldPrometheus { unPrometheus = x }) unPrometheus

initializeMetrics :: forall r d m. (MonadIO m, MonadJobs d (Scheduler r d m), PrometheusJob d) => r d -> m (Prometheus r d)
initializeMetrics base =
  Prometheus
    <$> pure base
    <*> register gauges
    <*> register counters
    <*> register summaries

  where
    counters  = vector "counter"          $ counter (Info "scheduler_operations" "Scheduler operation counters")
    gauges    = vector "gauge"            $ gauge   (Info "scheduler" "Scheduler gauges")
    summaries = vector (jobLabelFields d) $ summary (Info "job_durations" "Job execution times") defaultQuantiles
    d         = Proxy :: Proxy d

trackQueueDepth :: (MonadIO m, MonadJobs d (Scheduler r d m), PrometheusJob d) => Scheduler (Prometheus r) d m ()
trackQueueDepth = do
  gauges <- gets pGauges
  depth  <- fromIntegral . length <$> enumerate

  withLabel gauges "queue_depth" (`setGauge` depth)

trackNextJobTime :: (MonadIO m, MonadJobs d (Scheduler r d m), PrometheusJob d) => Scheduler (Prometheus r) d m ()
trackNextJobTime =
  stack peekQueue >>= \case
    Nothing -> return ()
    Just (time, _) -> do
      gauges <- gets pGauges

      withLabel gauges "next_execution_time" (`setGauge` asDouble time)

  where
    asDouble (ScheduledTime time) = realToFrac (utcTimeToPOSIXSeconds time)

incCounterL :: (Label l, MonadMonitor m) => l -> Vector l Counter -> m ()
incCounterL label vec = withLabel vec label incCounter

timeAction :: MonadIO m => m () -> m NominalDiffTime
timeAction action = do
  start <- liftIO getCurrentTime
  action
  end <- liftIO getCurrentTime
  return (end `diffUTCTime` start)

instance MonadIO io => MonadMonitor (Scheduler (Prometheus r) d io) where
  doIO = liftIO . doIO

instance (Monad m, MonadIO m, MonadJobs d (Scheduler r d m), PrometheusJob d) => MonadJobs d (Scheduler (Prometheus r) d m) where
  type ExecutionMonad (Scheduler (Prometheus r) d m) = ExecutionMonad (Scheduler r d m)

  pushQueue executesAt item = do
    stack $ pushQueue executesAt item

    trackQueueDepth
    trackNextJobTime
    incCounterL "jobs_added" =<< gets pCounters

  peekQueue = do
    item <- stack peekQueue
    incCounterL "jobs_peeked" =<< gets pCounters
    return item

  dropQueue = do
    stack dropQueue

    trackQueueDepth
    trackNextJobTime
    incCounterL "jobs_deleted" =<< gets pCounters

  execute workUnit action = do
    summaries <- gets pSummaries
    executionTime <- timeAction (stack $ execute workUnit action)

    withLabel summaries (jobLabel workUnit) (`observe` realToFrac executionTime)
    trackQueueDepth
    incCounterL "jobs_executed" =<< gets pCounters

  enumerate = stack enumerate

withPrometheus :: (MonadIO io, MonadJobs d (Scheduler r d io), PrometheusJob d) => Scheduler (Prometheus r) d io () -> Scheduler r d io ()
withPrometheus = unstackM initializeMetrics
