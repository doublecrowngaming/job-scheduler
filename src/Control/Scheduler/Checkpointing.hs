{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}

module Control.Scheduler.Checkpointing (
  withLocalCheckpointing,
  withDynamoDBCheckpointing,
  onColdStart
) where

import           Control.Scheduler.Checkpointing.Class    (onColdStart)
import           Control.Scheduler.Checkpointing.DynamoDB
import           Control.Scheduler.Checkpointing.Local
