{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Control.Scheduler.Type (
  Scheduler,
  unScheduler,
  stack,
  unstack,
  RunnableScheduler(..),
  DiskCheckpointing(..)
) where

import           Control.Monad.Catch        (MonadCatch (..), MonadMask (..),
                                             MonadThrow (..))
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Strict (MonadState, StateT (..))
import           Control.Monad.Trans.Class  (MonadTrans (..))


newtype Scheduler r d m a = Scheduler { unScheduler :: StateT (r d) m a }
                            deriving (
                              Functor, Applicative, Monad,
                              MonadState (r d),
                              MonadThrow, MonadCatch, MonadMask, MonadIO, MonadTrans
                            )

newtype DiskCheckpointing r d = DiskCheckpointing { unDiskCheckpointing :: r d }

stack :: Monad m => Scheduler r d m a -> Scheduler (DiskCheckpointing r) d m a
stack (Scheduler rStateT) = Scheduler (xform rStateT)
  where
    xform :: Monad m => StateT (r d) m a -> StateT (DiskCheckpointing r d) m a
    xform (StateT s1mas1) = StateT $ (fmap . fmap) DiskCheckpointing . s1mas1 . unDiskCheckpointing

unstack :: Monad m => Scheduler (DiskCheckpointing r) d m a -> Scheduler r d m a
unstack (Scheduler dcStateT) = Scheduler (unxform dcStateT)
  where
    unxform :: Monad m => StateT (DiskCheckpointing r d) m a -> StateT (r d) m a
    unxform (StateT s2mas2) = StateT $ (fmap . fmap) unDiskCheckpointing . s2mas2 . DiskCheckpointing

class RunnableScheduler r where
  runScheduler :: Monad m => Scheduler r d m () -> m ()
