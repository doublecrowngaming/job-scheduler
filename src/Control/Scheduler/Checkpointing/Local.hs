{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Scheduler.Checkpointing.Local (
  withLocalCheckpointing
) where

import           Control.Monad.Catch                   (MonadCatch, handleIf)
import           Control.Monad.IO.Class                (MonadIO (..))
import           Control.Scheduler.Checkpointing.Class (Checkpointer (..),
                                                        Checkpointing,
                                                        runCheckpointer)
import           Control.Scheduler.Class               (MonadJobs (..))
import           Control.Scheduler.Type                (Scheduler)
import           Data.Aeson                            (FromJSON (..),
                                                        ToJSON (..),
                                                        eitherDecodeFileStrict',
                                                        encodeFile)
import           System.IO.Error                       (isDoesNotExistError)


type LocalFileConstraints r d m = (MonadIO m, MonadCatch m, ToJSON d, FromJSON d, MonadJobs d (Scheduler r d m))

localFileCheckpointer :: LocalFileConstraints r d m => FilePath -> Checkpointer m r d
localFileCheckpointer filename = Checkpointer{..}
  where
    writeCheckpoint = do
      enumeration <- enumerate

      liftIO $ encodeFile filename enumeration

    readCheckpoint =
      handleIf isDoesNotExistError
        (const $ return [])
        (do
          result   <- liftIO $ eitherDecodeFileStrict' filename

          case result of
            Left err  -> error err
            Right val -> return val
        )

withLocalCheckpointing :: LocalFileConstraints r d io => FilePath -> Scheduler (Checkpointing io r) d io () -> Scheduler r d io ()
withLocalCheckpointing = runCheckpointer . localFileCheckpointer
