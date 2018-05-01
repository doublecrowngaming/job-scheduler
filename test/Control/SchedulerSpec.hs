{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.SchedulerSpec (spec) where

import           Control.Monad.State   (MonadState, State, execState, gets,
                                        modify)
import           Control.Scheduler
import           Data.Time.Clock.POSIX (POSIXTime)

import           Test.Hspec
import           Test.QuickCheck

data TestTimerState jobtype = TestTimerState {
  clock            :: !POSIXTime,
  executionCounter :: !Int,
  jobsSeen         :: ![jobtype],
  sourceTimings    :: ![POSIXTime],
  callbackTracker  :: ![JobState jobtype]
} deriving (Eq, Show)

newtype TestTimer jobtype a = TestTimer { unTestTimer :: State (TestTimerState jobtype) a }
  deriving (Functor, Applicative, Monad, MonadState (TestTimerState jobtype))

instance Timer (TestTimer jobtype) where
  currentTime = gets clock
  timerSleep interval = modify $ \state -> state { clock = clock state + interval }

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

execTestTimer :: POSIXTime -> TestTimer jobtype a -> TestTimerState jobtype
execTestTimer initTime actions = execState (unTestTimer actions) (TestTimerState initTime 0 [] [] [])

instance Arbitrary POSIXTime where
  arbitrary = fromRational <$> arbitrary

spec :: Spec
spec = do
  describe "Timer class" $
    describe "sleepUntil" $
      it "suspends execution until the desired time" $ property $
        \initTime endTime ->
          (endTime > initTime) ==>
            execTestTimer initTime (sleepUntil endTime) === TestTimerState endTime 0 ([] :: [String]) [] []

  describe "Scheduler" $ do
    describe "whenReady" $ do
      let bookkeep src = incrementExecutionCounter >> observeSource src
      let succeed = return (Continue Ok)
      let dirsrc1 = Periodic 5 "foo"
          dirsrc2 = PeriodicAfter 3 5 "bar"

      it "runs its actions periodically" $
        execTestTimer 100 (
          runScheduler [dirsrc1, dirsrc2] $
            whenReady $ \src -> do
              counter <- getExecutionCounter
              case counter of
                10 -> return Halt
                _  -> bookkeep src >> succeed
        ) `shouldBe` TestTimerState {
                        clock = 125,
                        executionCounter = 10,
                        jobsSeen = ["foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar"],
                        sourceTimings = [100, 103, 105, 108, 110, 113, 115, 118, 120, 123],
                        callbackTracker = []
                     }

      it "executes pending actions as soon as possible after a long-running action completes" $
        execTestTimer 100 (
          runScheduler [dirsrc1] $
            whenReady $ \src -> do
              counter <- getExecutionCounter
              case counter of
                4 -> return Halt
                1 -> bookkeep src >> timerSleep 21 >> succeed
                _ -> bookkeep src >> succeed
        ) `shouldBe` TestTimerState {
                        clock = 135,
                        executionCounter = 4,
                        jobsSeen = replicate 4 "foo",
                        sourceTimings = [
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
        execTestTimer 100 (
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
                        clock = 110,
                        executionCounter = 2,
                        jobsSeen = [],
                        sourceTimings = [],
                        callbackTracker = [
                          JobState dirsrc (Just 100) (Just Ok) (Just 100),
                          JobState dirsrc (Just 105) (Just Error) (Just 105)
                        ]
                     }

    describe "submitJob" $
      context "for one-time jobs" $ do
        let oneOff1 = Once 3      "foo"
            oneOff2 = Once 7      "bar"
            oneOff3 = Immediately "baz"
            oneOff4 = After 33    "glorch"

        it "arranges for actions to be run once" $
          execTestTimer 2 (
            runScheduler [oneOff1, oneOff2, oneOff3, oneOff4] $
              whenReady $ \src -> do
                incrementExecutionCounter
                observeSource src

                return (Continue Ok)
          ) `shouldBe` TestTimerState {
                          clock = 35,
                          executionCounter = 4,
                          jobsSeen = ["baz", "foo", "bar", "glorch"],
                          sourceTimings = [2, 3, 7, 35],
                          callbackTracker = []
                       }

        it "allows actions to be scheduled from whenReady" $
          execTestTimer 0 (
            runScheduler [oneOff1, oneOff2] $
              whenReadyS $ \src -> do
                lift incrementExecutionCounter
                lift $ observeSource src

                case src of
                  "bar" -> submitJob (Once 9 "baz")
                  _     -> return ()

                return (Continue Ok)

          ) `shouldBe` TestTimerState {
                          clock = 9,
                          executionCounter = 3,
                          jobsSeen = ["foo", "bar", "baz"],
                          sourceTimings = [3, 7, 9],
                          callbackTracker = []
                       }
