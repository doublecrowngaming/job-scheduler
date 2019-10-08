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

import           Control.Monad.IO.Class    (MonadIO)
import           Control.Scheduler.Class   (MonadJobs (..))
import           Control.Scheduler.Logging (HasLogger, LogLevel (..), slog)
import           Control.Scheduler.Type    (Enrichment (..), Iso (..),
                                            Scheduler, stack, unstack)
import           Data.Text                 (Text, pack)


tshow :: Show a => a -> Text
tshow x = pack (show x)


newtype Tracing r d = Tracing { unTracing :: r d }

instance Enrichment (Tracing r d) (r d) where
  enrich = const $ Iso Tracing unTracing

instance (MonadIO m, MonadJobs d (Scheduler r d m), HasLogger m) => MonadJobs d (Scheduler (Tracing r) d m) where
  type ExecutionMonad (Scheduler (Tracing r) d m) = ExecutionMonad (Scheduler r d m)

  pushQueue executesAt item = do
    slog DEBUG "Scheduler:Tracing" "attempting pushQueue"
    stack $ pushQueue executesAt item
    slog DEBUG "Scheduler:Tracing" "executed pushQueue"

  peekQueue = do
    slog DEBUG "Scheduler:Tracing" "attempting peekQueue"
    x <- stack peekQueue
    slog DEBUG "Scheduler:Tracing" "executed peekQueue"
    return x

  dropQueue = do
    slog DEBUG "Scheduler:Tracing" "attempting dropQueue"
    stack dropQueue
    slog DEBUG "Scheduler:Tracing" "executed dropQueue"

  execute workUnit action = do
    slog DEBUG "Scheduler:Tracing" "attempting execute"
    stack $ execute workUnit action
    slog DEBUG "Scheduler:Tracing" "execution succeeded"

  enumerate = stack enumerate

withTracing :: (MonadIO m, MonadJobs d (Scheduler r d m), HasLogger m) => Scheduler (Tracing r) d m () -> Scheduler r d m ()
withTracing = unstack undefined



newtype DeepTracing r d = DeepTracing { unDeepTracing :: r d }

instance Enrichment (DeepTracing r d) (r d) where
  enrich = const $ Iso DeepTracing unDeepTracing

instance (MonadIO m, MonadJobs d (Scheduler r d m), HasLogger m, Show d) => MonadJobs d (Scheduler (DeepTracing r) d m) where
  type ExecutionMonad (Scheduler (DeepTracing r) d m) = ExecutionMonad (Scheduler r d m)
  pushQueue executesAt item = do
    slog DEBUG "Scheduler:DeepTracing" ("attempting to arrange for " <> tshow item <> " to be executed at " <> tshow executesAt)
    stack $ pushQueue executesAt item
    slog DEBUG "Scheduler:DeepTracing" ("arranged for " <> tshow item <> " to be executed at " <> tshow executesAt)

  peekQueue = do
    slog DEBUG "Scheduler:DeepTracing" "retrieving next work item"
    item <- stack peekQueue
    slog DEBUG "Scheduler:DeepTracing" ("retrieved " <> tshow item)
    return item

  dropQueue = do
    slog DEBUG "Scheduler:DeepTracing" "dropping head-of-line work item"

    item <- stack peekQueue
    stack dropQueue

    slog DEBUG "Scheduler:DeepTracing" ("dropped " <> tshow item)

  execute workUnit action = do
    slog DEBUG "Scheduler:DeepTracing" ("attempting to execute associated action for " <> tshow workUnit)
    stack $ execute workUnit action
    slog DEBUG "Scheduler:DeepTracing" ("execution succeeded for " <> tshow workUnit)

  enumerate = stack enumerate

withDeepTracing :: (MonadIO m, MonadJobs d (Scheduler r d m), HasLogger m) => Scheduler (DeepTracing r) d m () -> Scheduler r d m ()
withDeepTracing = unstack undefined



newtype WorkQueueLogging r d = WorkQueueLogging { unWorkQueueLogging :: r d }

instance Enrichment (WorkQueueLogging r d) (r d) where
  enrich = const $ Iso WorkQueueLogging unWorkQueueLogging

logWorkQueue :: (MonadIO m, MonadJobs d (Scheduler r d m), HasLogger m, Show d) => Scheduler (WorkQueueLogging r) d m ()
logWorkQueue = do
  q <- enumerate
  slog DEBUG "Scheduler:WorkQueueLogging" ("current work queue contents: " <> tshow q)

instance (MonadIO m, MonadJobs d (Scheduler r d m), HasLogger m, Show d) => MonadJobs d (Scheduler (WorkQueueLogging r) d m) where
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

  execute workUnit action = do
    stack $ execute workUnit action
    logWorkQueue

  enumerate = stack enumerate

withWorkQueueLogging :: (MonadIO m, MonadJobs d (Scheduler r d m), HasLogger m) => Scheduler (WorkQueueLogging r) d m () -> Scheduler r d m ()
withWorkQueueLogging = unstack undefined
