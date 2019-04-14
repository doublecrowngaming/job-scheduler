{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE ScopedTypeVariables   #-}

module Control.Scheduler.Enrichments.Tracing (
  Tracing,
  withTracing,
  DeepTracing,
  withDeepTracing,
  WorkQueueLogging,
  withWorkQueueLogging
) where

import           Control.Monad.Logger    (MonadLogger, logDebugNS)
import           Control.Scheduler.Class (MonadJobs (..))
import           Control.Scheduler.Type  (Enrichment (..), Iso (..), Scheduler,
                                          stack, unstack)
import           Data.Text               (Text, pack)


tshow :: Show a => a -> Text
tshow x = pack (show x)


newtype Tracing r d = Tracing { unTracing :: r d }

instance Enrichment (Tracing r d) (r d) where
  enrich = const $ Iso Tracing unTracing

instance (Monad m, MonadJobs d (Scheduler r d m), MonadLogger m) => MonadJobs d (Scheduler (Tracing r) d m) where
  type ExecutionMonad (Scheduler (Tracing r) d m) = ExecutionMonad (Scheduler r d m)

  pushQueue executesAt item = do
    logDebugNS "Scheduler:Tracing" "attempting pushQueue"
    stack $ pushQueue executesAt item
    logDebugNS "Scheduler:Tracing" "executed pushQueue"

  peekQueue = do
    logDebugNS "Scheduler:Tracing" "attempting peekQueue"
    x <- stack peekQueue
    logDebugNS "Scheduler:Tracing" "executed peekQueue"
    return x

  dropQueue = do
    logDebugNS "Scheduler:Tracing" "attempting dropQueue"
    stack dropQueue
    logDebugNS "Scheduler:Tracing" "executed dropQueue"

  execute action = do
    logDebugNS "Scheduler:Tracing" "attempting execute"
    stack $ execute action
    logDebugNS "Scheduler:Tracing" "execution succeeded"

  enumerate = stack enumerate

withTracing :: (MonadJobs d (Scheduler r d m), MonadLogger m) => Scheduler (Tracing r) d m () -> Scheduler r d m ()
withTracing = unstack undefined



newtype DeepTracing r d = DeepTracing { unDeepTracing :: r d }

instance Enrichment (DeepTracing r d) (r d) where
  enrich = const $ Iso DeepTracing unDeepTracing

instance (Monad m, MonadJobs d (Scheduler r d m), MonadLogger m, Show d) => MonadJobs d (Scheduler (DeepTracing r) d m) where
  type ExecutionMonad (Scheduler (DeepTracing r) d m) = ExecutionMonad (Scheduler r d m)
  pushQueue executesAt item = do
    logDebugNS "Scheduler:DeepTracing" ("attempting to arrange for " <> tshow item <> " to be executed at " <> tshow executesAt)
    stack $ pushQueue executesAt item
    logDebugNS "Scheduler:DeepTracing" ("arranged for " <> tshow item <> " to be executed at " <> tshow executesAt)

  peekQueue = do
    logDebugNS "Scheduler:DeepTracing" "retrieving next work item"
    item <- stack peekQueue
    logDebugNS "Scheduler:DeepTracing" ("retrieved " <> tshow item)
    return item

  dropQueue = do
    logDebugNS "Scheduler:DeepTracing" "dropping head-of-line work item"

    item <- stack peekQueue
    stack dropQueue

    logDebugNS "Scheduler:DeepTracing" ("dropped " <> tshow item)

  execute action = do
    logDebugNS "Scheduler:DeepTracing" "attempting to execute associated action"
    stack $ execute action
    logDebugNS "Scheduler:DeepTracing" "execution succeeded"

  enumerate = stack enumerate

withDeepTracing :: (MonadJobs d (Scheduler r d m), MonadLogger m) => Scheduler (DeepTracing r) d m () -> Scheduler r d m ()
withDeepTracing = unstack undefined



newtype WorkQueueLogging r d = WorkQueueLogging { unWorkQueueLogging :: r d }

instance Enrichment (WorkQueueLogging r d) (r d) where
  enrich = const $ Iso WorkQueueLogging unWorkQueueLogging

logWorkQueue :: (Monad m, MonadJobs d (Scheduler r d m), MonadLogger m, Show d) => Scheduler (WorkQueueLogging r) d m ()
logWorkQueue = do
  q <- enumerate
  logDebugNS "Scheduler:WorkQueueLogging" ("current work queue contents: " <> tshow q)

instance (Monad m, MonadJobs d (Scheduler r d m), MonadLogger m, Show d) => MonadJobs d (Scheduler (WorkQueueLogging r) d m) where
  type ExecutionMonad (Scheduler (WorkQueueLogging r) d m) = ExecutionMonad (Scheduler r d m)
  pushQueue executesAt item = do
    stack $ pushQueue executesAt item
    logWorkQueue

  peekQueue = do
    item <- stack peekQueue
    logWorkQueue
    return item

  dropQueue = do
    stack dropQueue
    logWorkQueue

  execute action = do
    stack $ execute action
    logWorkQueue

  enumerate = stack enumerate

withWorkQueueLogging :: (MonadJobs d (Scheduler r d m), MonadLogger m) => Scheduler (WorkQueueLogging r) d m () -> Scheduler r d m ()
withWorkQueueLogging = unstack undefined
