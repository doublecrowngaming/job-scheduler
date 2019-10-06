{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Scheduler.Checkpointing.DynamoDB (
  withDynamoDBCheckpointing
) where

import           Control.Monad                         (void, when)
import           Control.Monad.Catch                   (MonadCatch)
import           Control.Monad.IO.Class                (MonadIO (..))
import           Control.Scheduler.Checkpointing.Class (Checkpointer (..),
                                                        Checkpointing,
                                                        runCheckpointer)
import           Control.Scheduler.Class               (MonadJobs (..))
import           Control.Scheduler.Logging             (HasLogger (..),
                                                        LogLevel (..),
                                                        Logger (..))
import           Control.Scheduler.Type                (Scheduler)
import           Data.Aeson                            (FromJSON (..),
                                                        ToJSON (..),
                                                        eitherDecode, encode)
import           Data.ByteString.Lazy                  (fromStrict, toStrict)
import           Data.Text                             (Text)
import           Lens.Micro                            (at, (&), (.~), (?~),
                                                        (^.), _Just)
import           Lens.Micro.Platform                   ()
import           Network.AWS                           (Credentials (Discover),
                                                        Env, envLogger, newEnv,
                                                        runAWS, runResourceT,
                                                        send)
import qualified Network.AWS                           as AWS (LogLevel (..))
import           Network.AWS.DynamoDB                  (attributeValue, avB,
                                                        avS, getItem, giKey,
                                                        girsItem, piItem,
                                                        putItem)


dynamoCheckpointer :: (MonadIO m, MonadCatch m, ToJSON d, FromJSON d, MonadJobs d (Scheduler r d m)) => Env -> Text -> Text -> Checkpointer m r d
dynamoCheckpointer env tableName key = Checkpointer{..}
  where
    writeCheckpoint = do
      enumeration  <- enumerate

      let encoded    = toStrict $ encode enumeration
          putItemCmd = putItem tableName
                        & piItem .~ [
                            ("key",  attributeValue & avS ?~ key),
                            ("data",    attributeValue & avB ?~ encoded)
                        ]

      void . liftIO . runResourceT . runAWS env $ send putItemCmd

    readCheckpoint = do
      let getItemCmd = getItem tableName & giKey .~ [("key",  attributeValue & avS ?~ key)]

      resp <- liftIO . runResourceT . runAWS env $ send getItemCmd

      case resp^.girsItem.at "data"._Just.avB of
        Nothing      -> return []
        Just encoded ->
          case eitherDecode . fromStrict $ encoded of
            Left err  -> error err
            Right val -> return val

withDynamoDBCheckpointing :: (ToJSON d, FromJSON d, Show d, MonadJobs d (Scheduler r d io), MonadCatch io, HasLogger io, MonadIO io) => Text -> Text -> LogLevel -> Scheduler (Checkpointing io r) d io () -> Scheduler r d io ()
withDynamoDBCheckpointing tableName key minLogLevel actions = do
  logger <- convertLogger =<< getLogger
  env <- newEnv Discover

  let env' = env & envLogger .~ logger

  runCheckpointer (dynamoCheckpointer env' tableName key) actions

  where
    convertLogLevel AWS.Info  = INFO
    convertLogLevel AWS.Error = ERROR
    convertLogLevel AWS.Trace = TRACE
    convertLogLevel AWS.Debug = DEBUG

    convertLogger (Logger logger) =
      return $ \lvl builder -> do
        let logLevel = convertLogLevel lvl
        when (logLevel >= minLogLevel) $
          logger logLevel "Scheduler:Checkpointing:DynamoDB" builder
