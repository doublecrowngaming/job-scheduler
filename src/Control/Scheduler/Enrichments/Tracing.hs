{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Scheduler.Enrichments.Tracing (
  Tracing,
  withTracing,
  DeepTracing,
  withDeepTracing,
  WorkQueueLogging,
  withWorkQueueLogging
) where

import           Control.Monad.IO.Class  (MonadIO (..))
import           Control.Monad.Logger    (MonadLogger, logDebugNS)
import           Control.Scheduler.Class (MonadJobs (..))
import           Control.Scheduler.Type  (Enrichment (..), Iso (..),
                                          RunnableScheduler (..), Scheduler,
                                          stack, unstack)
import           Data.Text               (Text, pack)


tshow :: Show a => a -> Text
tshow x = pack (show x)


newtype Tracing r d = Tracing { unTracing :: r d }

instance Enrichment (Tracing r d) (r d) where
  enrich = const $ Iso Tracing unTracing
  strip  = Iso unTracing Tracing

instance (Monad m, MonadJobs d (Scheduler r d m), MonadLogger m) => MonadJobs d (Scheduler (Tracing r) d m) where
  pushQueue executesAt item = do
    logDebugNS "Scheduler:Tracing" "attempting pushQueue"
    stack $ pushQueue executesAt item
    logDebugNS "Scheduler:Tracing" "executed pushQueue"

  popQueue = do
    logDebugNS "Scheduler:Tracing" "attempting popQueue"
    x <- stack popQueue
    logDebugNS "Scheduler:Tracing" "executed popQueue"
    return x

  execute action = do
    logDebugNS "Scheduler:Tracing" "attempting execute"
    stack . execute $ unstack action
    logDebugNS "Scheduler:Tracing" "execution succeeded"

  enumerate = stack enumerate

withTracing :: (MonadJobs d (Scheduler r d m), MonadLogger m) => Scheduler (Tracing r) d m () -> Scheduler r d m ()
withTracing = unstack



newtype DeepTracing r d = DeepTracing { unDeepTracing :: r d }

instance Enrichment (DeepTracing r d) (r d) where
  enrich = const $ Iso DeepTracing unDeepTracing
  strip  = Iso unDeepTracing DeepTracing

instance (Monad m, MonadJobs d (Scheduler r d m), MonadLogger m, Show d) => MonadJobs d (Scheduler (DeepTracing r) d m) where
  pushQueue executesAt item = do
    logDebugNS "Scheduler:DeepTracing" ("attempting to arrange for " <> tshow item <> " to be executed at " <> tshow executesAt)
    stack $ pushQueue executesAt item
    logDebugNS "Scheduler:DeepTracing" ("arranged for " <> tshow item <> " to be executed at " <> tshow executesAt)

  popQueue = do
    logDebugNS "Scheduler:DeepTracing" "retrieving next work item"
    item <- stack popQueue
    logDebugNS "Scheduler:DeepTracing" ("retrieved " <> tshow item)
    return item

  execute action = do
    logDebugNS "Scheduler:DeepTracing" "attempting to execute associated action"
    stack . execute $ unstack action
    logDebugNS "Scheduler:DeepTracing" "execution succeeded"

  enumerate = stack enumerate

withDeepTracing :: (MonadJobs d (Scheduler r d m), MonadLogger m) => Scheduler (DeepTracing r) d m () -> Scheduler r d m ()
withDeepTracing = unstack



newtype WorkQueueLogging r d = WorkQueueLogging { unWorkQueueLogging :: r d }

instance Enrichment (WorkQueueLogging r d) (r d) where
  enrich = const $ Iso WorkQueueLogging unWorkQueueLogging
  strip  = Iso unWorkQueueLogging WorkQueueLogging

logWorkQueue :: (Monad m, MonadJobs d (Scheduler r d m), MonadLogger m, Show d) => Scheduler (WorkQueueLogging r) d m ()
logWorkQueue = do
  q <- enumerate
  logDebugNS "Scheduler:WorkQueueLogging" ("current work queue contents: " <> tshow q)

instance (Monad m, MonadJobs d (Scheduler r d m), MonadLogger m, Show d) => MonadJobs d (Scheduler (WorkQueueLogging r) d m) where
  pushQueue executesAt item = do
    stack $ pushQueue executesAt item
    logWorkQueue

  popQueue = do
    item <- stack popQueue
    logWorkQueue
    return item

  execute action = do
    stack . execute $ unstack action
    logWorkQueue

  enumerate = stack enumerate

withWorkQueueLogging :: (MonadJobs d (Scheduler r d m), MonadLogger m) => Scheduler (WorkQueueLogging r) d m () -> Scheduler r d m ()
withWorkQueueLogging = unstack
