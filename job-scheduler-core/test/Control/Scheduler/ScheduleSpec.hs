{-# LANGUAGE ScopedTypeVariables #-}

module Control.Scheduler.ScheduleSpec (spec) where

import           Control.Scheduler hiding (now)
import           Data.Maybe        (isNothing)
import           Data.Time.Clock   (DiffTime, UTCTime (..))
import           System.Cron       (daily)

import           Test.Hspec
import           Test.QuickCheck


newtype ArbitraryDelay = ArbitraryDelay Delay deriving Show

instance Arbitrary ArbitraryDelay where
  arbitrary = ArbitraryDelay . fromInteger <$> arbitrary

newtype ArbitraryInterval = ArbitraryInterval Interval deriving Show

instance Arbitrary ArbitraryInterval where
  arbitrary = ArbitraryInterval . fromInteger <$> arbitrary

newtype ArbitraryTime = ArbitraryTime UTCTime deriving Show

instance Arbitrary ArbitraryTime where
  arbitrary = (ArbitraryTime .) . UTCTime
                <$> (toEnum <$> arbitrary)
                <*> (fromInteger <$> arbitrary)

replaceTime :: UTCTime -> DiffTime -> UTCTime
replaceTime (UTCTime day _) = UTCTime day

spec :: Spec
spec = do
  describe "After" $ do
    it "always runs after a given delay" $ property $
      \(ArbitraryTime timestamp, ArbitraryDelay delay) ->
        runAt (After delay) (CurrentTime timestamp) === Just (ScheduledTime (timestamp `addTime` delay))

    it "produces no successor jobs" $ property $
      \(ArbitraryTime timestamp, ArbitraryDelay delay) ->
        isNothing $ nextJob (After delay) (CurrentTime timestamp)


  describe "At" $ do
    it "runs at a given time" $ property $
      \(ArbitraryTime now, ArbitraryTime runtime) ->
        let scheduledTime = ScheduledTime runtime
            currentTime   = CurrentTime now
        in
          runAt (At scheduledTime) currentTime === Just scheduledTime

    it "produces no successor jobs" $ property $
      \(ArbitraryTime now, ArbitraryTime runtime) ->
        let scheduledTime = ScheduledTime runtime
            currentTime   = CurrentTime now
        in
          isNothing $ nextJob (At scheduledTime) currentTime


  describe "Cron" $ do
    it "always runs after a given delay" $ property $
      \(ArbitraryTime now) ->
        let now'       = CurrentTime now
            plusOneDay = now `addTime` (86400 :: Delay)
        in
          runAt (Cron daily) now' === Just (ScheduledTime $ replaceTime plusOneDay 0)

    it "produces a successor job" $ property $
      \(ArbitraryTime now) ->
        nextJob (Cron daily) (CurrentTime now) === Just (Cron daily)


  describe "Every" $ do
    it "always runs after a given delay" $ property $
      \(ArbitraryTime now, ArbitraryInterval interval, ArbitraryDelay delay) ->
        let now' = CurrentTime now
        in
          runAt (Every delay interval) now' === Just (ScheduledTime (now `addTime` delay))

    it "produces a successor job" $
      pendingWith "Every's nextJob result involves a hidden constructor"


  describe "Immediately" $ do
    it "always wants to run without delay" $ property $
      \(ArbitraryTime timestamp) -> runAt Immediately (CurrentTime timestamp) === Just (ScheduledTime timestamp)

    it "produces no successor jobs" $ property $
      \(ArbitraryTime timestamp) -> isNothing $ nextJob Immediately (CurrentTime timestamp)
