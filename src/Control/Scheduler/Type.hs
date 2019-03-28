{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Control.Scheduler.Type (
  Scheduler,
  unScheduler,
  RunnableScheduler(..)
) where

import           Control.Monad.Catch        (MonadCatch (..), MonadMask (..),
                                             MonadThrow (..))
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Strict (MonadState, StateT)
import           Control.Monad.Trans.Class  (MonadTrans (..))


newtype Scheduler s d m a = Scheduler { unScheduler :: StateT (s d) m a }
                            deriving (
                              Functor, Applicative, Monad,
                              MonadState (s d),
                              MonadThrow, MonadCatch, MonadMask, MonadIO, MonadTrans
                            )

class RunnableScheduler s where
  runScheduler :: Monad m => Scheduler s d m () -> m ()
