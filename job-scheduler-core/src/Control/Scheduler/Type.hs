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
  unstackM,
  withScheduler,
  RunnableScheduler(..),
  Iso(..),
  Enrichment(..)
) where

import           Control.Monad.Catch        (MonadCatch (..), MonadMask (..),
                                             MonadThrow (..))
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Logger       (MonadLogger, MonadLoggerIO)
import           Control.Monad.State.Strict (MonadState, StateT (..), get)
import           Control.Monad.Trans.Class  (MonadTrans (..))


newtype Scheduler r d m a = Scheduler { unScheduler :: StateT (r d) m a }
                              deriving (
                                Functor, Applicative, Monad,
                                MonadState (r d),
                                MonadThrow, MonadCatch, MonadMask, MonadIO, MonadTrans,
                                MonadLogger, MonadLoggerIO
                              )

class RunnableScheduler r where
  runScheduler :: Monad m => Scheduler r d m () -> m ()


data Iso a b = Iso { forward :: a -> b, reverse :: b -> a }

inverse :: Iso a b -> Iso b a
inverse (Iso f r) = Iso r f


class Enrichment composite base where
  enrich :: composite -> Iso base composite


withStateT' :: Functor m => Iso s1 s2 -> StateT s1 m a -> StateT s2 m a
withStateT' (Iso fwd rev) (StateT s1mas1) = StateT $ (fmap . fmap) fwd . s1mas1 . rev

withScheduler :: Functor m => Iso (s1 d) (s2 d) -> Scheduler s1 d m a -> Scheduler s2 d m a
withScheduler iso = Scheduler . withStateT' iso . unScheduler


stack :: (Enrichment (k r d) (r d), Monad m) => Scheduler r d m a -> Scheduler (k r) d m a
stack action = do
  state <- get
  withScheduler (enrich state) action

unstack :: (Enrichment (k r d) (r d), Monad m) => (r d -> k r d) -> Scheduler (k r) d m a -> Scheduler r d m a
unstack mkComposite action = do
  composite <- mkComposite <$> get
  withScheduler (inverse $ enrich composite) action

unstackM :: (Enrichment (k r d) (r d), Monad m) => (r d -> m (k r d)) -> Scheduler (k r) d m a -> Scheduler r d m a
unstackM mkCompositeM action = do
  composite <- (lift . mkCompositeM) =<< get
  withScheduler (inverse $ enrich composite) action
