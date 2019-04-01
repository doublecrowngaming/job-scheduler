{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

module Control.SchedulerSpec (spec) where

import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Scheduler.Chronometer
import           Control.Scheduler.Class
import           Control.Scheduler.Runner.SingleThreaded
import           Control.Scheduler.Task
import           Control.Scheduler.Time                  (CurrentTime (..),
                                                          ScheduledTime (..))
import           Control.Scheduler.Type
import           Data.Time.Clock                         (UTCTime)
import           System.Cron                             (yearly)

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

testScheduler :: Scheduler SingleThreaded String EffectLogger () -> [ExecutedAction]
testScheduler = execEffectLogger (read "1970-01-01 00:00:00") . runScheduler @SingleThreaded

loggingReactor :: Scheduler SingleThreaded String EffectLogger ()
loggingReactor =
  react $ \datum -> do
    now' <- now
    lift $ tell [ExecutedAction now' datum]

spec :: Spec
spec = do
  describe "Scheduler" $
    it "runs jobs until it has drained its work queue" $ do
      let history = testScheduler $ do
                      schedule $ Immediately "foo"

                      react $ \datum -> do
                        now' <- now
                        lift $ tell [ExecutedAction now' datum]

      history `shouldMatchList` [
          ExecutedAction (CurrentTime (read "1970-01-01 00:00:00")) "foo"
        ]

  describe "Task types" $ do
    describe "Immediately" $
      it "runs as soon as the reactor starts" $ do
        let history = testScheduler $ do
                        schedule $ Immediately "foobar"

                        sleepUntil (ScheduledTime (read "1980-01-01 12:34:56"))

                        loggingReactor

        history `shouldBe` [
            ExecutedAction (CurrentTime (read "1980-01-01 12:34:56")) "foobar"
          ]


    describe "At" $
      it "runs at a given time" $ do
        let history = testScheduler $ do
                        schedule $ Immediately "immediate"
                        schedule $ At (ScheduledTime (read "1970-01-02 03:04:05")) "atjob"

                        loggingReactor

        history `shouldBe` [
            ExecutedAction (CurrentTime (read "1970-01-01 00:00:00")) "immediate",
            ExecutedAction (CurrentTime (read "1970-01-02 03:04:05")) "atjob"
          ]

    describe "After" $
      it "runs after a given delay" $ do
        let history = testScheduler $ do
                        schedule $ Immediately "immediate"
                        schedule $ After 30 "afterjob"

                        loggingReactor

        history `shouldBe` [
            ExecutedAction (CurrentTime (read "1970-01-01 00:00:00")) "immediate",
            ExecutedAction (CurrentTime (read "1970-01-01 00:00:30")) "afterjob"
          ]

    describe "Every" $
      it "runs every N seconds" $ do
        let history = testScheduler $ do
                        setSchedulerEndTime (ScheduledTime (read "1970-01-01 00:03:00"))
                        schedule $ Immediately "immediate"
                        schedule $ Every 30 30 "every"

                        loggingReactor

        history `shouldBe` [
            ExecutedAction (CurrentTime (read "1970-01-01 00:00:00")) "immediate",
            ExecutedAction (CurrentTime (read "1970-01-01 00:00:30")) "every",
            ExecutedAction (CurrentTime (read "1970-01-01 00:01:00")) "every",
            ExecutedAction (CurrentTime (read "1970-01-01 00:01:30")) "every",
            ExecutedAction (CurrentTime (read "1970-01-01 00:02:00")) "every",
            ExecutedAction (CurrentTime (read "1970-01-01 00:02:30")) "every",
            ExecutedAction (CurrentTime (read "1970-01-01 00:03:00")) "every"
          ]

    describe "Cron" $
      it "runs according to a cron schedule" $ do
        let history = testScheduler $ do
                        setSchedulerEndTime (ScheduledTime (read "1980-01-01 00:00:00"))
                        schedule $ Cron yearly "cron"

                        loggingReactor

        history `shouldBe` [
            ExecutedAction (CurrentTime (read "1971-01-01 00:00:00")) "cron",
            ExecutedAction (CurrentTime (read "1972-01-01 00:00:00")) "cron",
            ExecutedAction (CurrentTime (read "1973-01-01 00:00:00")) "cron",
            ExecutedAction (CurrentTime (read "1974-01-01 00:00:00")) "cron",
            ExecutedAction (CurrentTime (read "1975-01-01 00:00:00")) "cron",
            ExecutedAction (CurrentTime (read "1976-01-01 00:00:00")) "cron",
            ExecutedAction (CurrentTime (read "1977-01-01 00:00:00")) "cron",
            ExecutedAction (CurrentTime (read "1978-01-01 00:00:00")) "cron",
            ExecutedAction (CurrentTime (read "1979-01-01 00:00:00")) "cron",
            ExecutedAction (CurrentTime (read "1980-01-01 00:00:00")) "cron"
          ]
