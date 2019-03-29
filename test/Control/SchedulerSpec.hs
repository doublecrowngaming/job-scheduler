{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

module Control.SchedulerSpec (spec) where

import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Scheduler.Chronometer
import           Control.Scheduler.Class
import           Control.Scheduler.Runner.SingleThreaded
import           Control.Scheduler.Task.Immediately
import           Control.Scheduler.Time                  (CurrentTime (..),
                                                          ScheduledTime (..))
import           Control.Scheduler.Type
import           Data.Time.Clock                         (UTCTime)

import           Test.Hspec


newtype PureTime a = PureTime { runPureTime :: State UTCTime a }
  deriving (Functor, Applicative, Monad)

instance MonadChronometer PureTime where
  now                             = PureTime (gets CurrentTime)
  sleepUntil (ScheduledTime time) = PureTime $ do
                                      now' <- get
                                      when (time > now') $
                                        put time

data ExecutedAction = ExecutedAction CurrentTime String deriving (Eq, Show)

newtype EffectLogger a = EffectLogger { runEffectLogger :: WriterT [ExecutedAction] PureTime a }
  deriving (Functor, Applicative, Monad, MonadWriter [ExecutedAction], MonadChronometer)

execEffectLogger :: UTCTime -> EffectLogger a -> [ExecutedAction]
execEffectLogger startTime action =
  evalState (
    runPureTime (
      execWriterT (runEffectLogger action)
    )
  ) startTime

spec :: Spec
spec =
  describe "Scheduler" $
    it "runs jobs until it has drained its work queue" $ do
      let history = execEffectLogger (read "1970-01-01 00:00:00") . runScheduler @SingleThreaded $ do
                      schedule $ Immediately "foo"

                      react $ \datum -> do
                        now' <- now
                        lift $ tell [ExecutedAction now' datum]

      history `shouldMatchList` [
          ExecutedAction (CurrentTime (read "1970-01-01 00:00:00")) "foo"
        ]
