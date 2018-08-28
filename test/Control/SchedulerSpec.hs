{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.SchedulerSpec (spec) where

import           Control.Monad.State    (MonadState, State, execState, gets,
                                         modify)
import           Control.Scheduler
import           Control.Scheduler.Time
import           Data.Time.Clock        (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX  (posixSecondsToUTCTime)

import           Test.Hspec
import           Test.QuickCheck

data TestTimerState jobtype = TestTimerState {
  clock            :: !UTCTime,
  executionCounter :: !Int,
  jobsSeen         :: ![jobtype],
  sourceTimings    :: ![UTCTime],
  callbackTracker  :: ![JobState jobtype]
} deriving (Eq, Show)

newtype TestTimer jobtype a = TestTimer { unTestTimer :: State (TestTimerState jobtype) a }
  deriving (Functor, Applicative, Monad, MonadState (TestTimerState jobtype))

instance Timer (TestTimer jobtype) where
  currentTime = gets clock
  timerSleep delay' = modify $ \state -> state { clock = clock state `addDelay` delay' }

incrementExecutionCounter :: TestTimer jobtype ()
incrementExecutionCounter = modify $ \state -> state { executionCounter = executionCounter state + 1 }

getExecutionCounter :: TestTimer jobtype Int
getExecutionCounter = gets executionCounter

logCallbackOutput :: JobState jobtype -> TestTimer jobtype ()
logCallbackOutput source  = modify $ \oldState ->
  oldState {
    callbackTracker = callbackTracker oldState ++ [source]
  }

observeSource :: jobtype -> TestTimer jobtype ()
observeSource datasource = modify $ \state ->
  state {
    jobsSeen = jobsSeen state ++ [datasource],
    sourceTimings = sourceTimings state ++ [clock state]
  }

execTestTimer :: UTCTime -> TestTimer jobtype a -> TestTimerState jobtype
execTestTimer initTime actions = execState (unTestTimer actions) (TestTimerState initTime 0 [] [] [])

instance Arbitrary UTCTime where
  arbitrary = posixSecondsToUTCTime . (realToFrac :: Double -> NominalDiffTime) <$> arbitrary

utc :: Integer -> UTCTime
utc = posixSecondsToUTCTime . fromInteger

spec :: Spec
spec = do
  describe "Timer class" $
    describe "sleepUntil" $
      it "suspends execution until the desired time" $ property $
        \initTime endTime ->
          (endTime > initTime) ==>
            execTestTimer initTime (sleepUntil endTime) === TestTimerState endTime 0 ([] :: [String]) [] []

  describe "Scheduler" $ do
    let bookkeep src = incrementExecutionCounter >> observeSource src
    let succeed = return (Continue Ok)

    describe "whenReady" $ do
      let dirsrc1 = Periodic 5 "foo"
          dirsrc2 = PeriodicAfter 17 4 "bar"

      it "runs Periodic jobs" $
        execTestTimer (utc 100) (
          runScheduler [dirsrc1] $
            whenReady $ \src -> do
              counter <- getExecutionCounter
              case counter of
                10 -> return Halt
                _  -> bookkeep src >> succeed
        ) `shouldBe` TestTimerState {
                        clock = utc 150,
                        executionCounter = 10,
                        jobsSeen = ["foo", "foo", "foo", "foo", "foo", "foo", "foo", "foo", "foo", "foo"],
                        sourceTimings = map utc [100, 105, 110, 115, 120, 125, 130, 135, 140, 145],
                        callbackTracker = []
                     }
      it "runs PeriodicAfter jobs" $
        execTestTimer (utc 100) (
          runScheduler [dirsrc2] $
            whenReady $ \src -> do
              counter <- getExecutionCounter
              case counter of
                10 -> return Halt
                _  -> bookkeep src >> succeed
        ) `shouldBe` TestTimerState {
                        clock = utc 157,
                        executionCounter = 10,
                        jobsSeen = ["bar", "bar", "bar", "bar", "bar", "bar", "bar", "bar", "bar", "bar"],
                        sourceTimings = map utc [117, 121, 125, 129, 133, 137, 141, 145, 149, 153],
                        callbackTracker = []
                    }

      it "runs mixed Periodic and PeriodicAfter jobs" $
        execTestTimer (utc 100) (
          runScheduler [dirsrc1, dirsrc2] $
            whenReady $ \src -> do
              counter <- getExecutionCounter
              case counter of
                10 -> return Halt
                _  -> bookkeep src >> succeed
        ) `shouldBe` TestTimerState {
                        clock = utc 130,
                        executionCounter = 10,
                        jobsSeen = ["foo", "foo", "foo", "foo", "bar", "foo", "bar", "bar", "foo", "bar"],
                        sourceTimings = map utc [100, 105, 110, 115, 117, 120, 121, 125, 125, 129],
                        callbackTracker = []
                      }
      it "executes pending actions as soon as possible after a long-running action completes" $
        execTestTimer (utc 100) (
          runScheduler [dirsrc1] $
            whenReady $ \src -> do
              counter <- getExecutionCounter
              case counter of
                4 -> return Halt
                1 -> bookkeep src >> timerSleep 21 >> succeed
                _ -> bookkeep src >> succeed
        ) `shouldBe` TestTimerState {
                        clock = utc 135,
                        executionCounter = 4,
                        jobsSeen = replicate 4 "foo",
                        sourceTimings = map utc [
                          100,
                          105,
                          126,
                          130
                        ],
                        callbackTracker = []
                     }

    describe "onStatChange" $ do
      let dirsrc = Periodic 5 "foo"

      it "registers a callback that is called when an action completes" $
        execTestTimer (utc 100) (
          runScheduler [dirsrc] $ do
            onStateChange logCallbackOutput
            whenReady $ \_ -> do
              counter <- getExecutionCounter
              case counter of
                2 -> return Halt
                1 -> incrementExecutionCounter >> return (Continue Error)
                0 -> incrementExecutionCounter >> return (Continue Ok)
                _ -> error "can't happen"
        ) `shouldBe` TestTimerState {
                        clock = utc 110,
                        executionCounter = 2,
                        jobsSeen = [],
                        sourceTimings = [],
                        callbackTracker = [
                          JobState dirsrc (Just $ utc 100) (Just Ok)    (Just $ utc 100) (ReferenceTime $ utc 100),
                          JobState dirsrc (Just $ utc 105) (Just Error) (Just $ utc 105) (ReferenceTime $ utc 100)
                        ]
                     }

    describe "submitJob" $
      context "for one-time jobs" $ do
        let oneOff1 = Once (utc 3) "foo"
            oneOff2 = Once (utc 7) "bar"
            oneOff3 = Immediately  "baz"
            oneOff4 = After 33     "glorch"

        it "arranges for actions to be run once" $
          execTestTimer (utc 2) (
            runScheduler [oneOff1, oneOff2, oneOff3, oneOff4] $
              whenReady $ \src -> do
                incrementExecutionCounter
                observeSource src

                return (Continue Ok)
          ) `shouldBe` TestTimerState {
                          clock = utc 35,
                          executionCounter = 4,
                          jobsSeen = ["baz", "foo", "bar", "glorch"],
                          sourceTimings = map utc [2, 3, 7, 35],
                          callbackTracker = []
                       }

        it "allows actions to be scheduled from whenReady" $
          execTestTimer (utc 0) (
            runScheduler [oneOff1, oneOff2] $
              whenReadyS $ \src -> do
                lift incrementExecutionCounter
                lift $ observeSource src

                case src of
                  "bar" -> submitJob (Once (utc 9) "baz")
                  _     -> return ()

                return (Continue Ok)

          ) `shouldBe` TestTimerState {
                          clock = utc 9,
                          executionCounter = 3,
                          jobsSeen = ["foo", "bar", "baz"],
                          sourceTimings = map utc [3, 7, 9],
                          callbackTracker = []
                       }

    describe "removeJob" $ do
      it "removes an Exact job" $
        execTestTimer (utc 0) (
          runScheduler [Once (utc 3) "bar", Once (utc 3) "foo", Once (utc 5) "foo"] $ do
            removeJob (Exact $ Once (utc 3) "foo")
            whenReady $ \src -> bookkeep src >> succeed
        ) `shouldBe` TestTimerState {
          clock            = utc 5,
          executionCounter = 2,
          jobsSeen         = ["bar", "foo"],
          sourceTimings    = map utc [3, 5],
          callbackTracker  = []
        }

      it "removes a job ByContent" $
        execTestTimer (utc 0) (
          runScheduler [Once (utc 3) "bar", Once (utc 3) "foo", Once (utc 5) "foo"] $ do
            removeJob (ByContent "foo")
            whenReady $ \src -> bookkeep src >> succeed
        ) `shouldBe` TestTimerState {
          clock            = utc 3,
          executionCounter = 1,
          jobsSeen         = ["bar"],
          sourceTimings    = map utc [3],
          callbackTracker  = []
        }