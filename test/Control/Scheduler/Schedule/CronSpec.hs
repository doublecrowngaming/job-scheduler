{-# LANGUAGE ScopedTypeVariables #-}

module Control.Scheduler.Schedule.CronSpec (spec) where

import           Control.Scheduler.Schedule
import           Control.Scheduler.Time     (CurrentTime (..), Delay, Interval,
                                             ScheduledTime (..), addTime,
                                             replaceTime)
import           Data.Functor.Identity
import           Data.Time.Clock            (UTCTime (..))
import           System.Cron                (daily)

import           Test.Hspec
import           Test.QuickCheck


newtype ArbitraryTime = ArbitraryTime UTCTime deriving Show

instance Arbitrary ArbitraryTime where
  arbitrary = (ArbitraryTime .) . UTCTime
                <$> (toEnum <$> arbitrary)
                <*> (fromInteger <$> arbitrary)

newtype ArbitraryDelay = ArbitraryDelay Delay deriving Show

instance Arbitrary ArbitraryDelay where
  arbitrary = ArbitraryDelay . fromInteger <$> arbitrary

newtype ArbitraryInterval = ArbitraryInterval Interval deriving Show

instance Arbitrary ArbitraryInterval where
  arbitrary = ArbitraryInterval . fromInteger <$> arbitrary

spec :: Spec
spec =
  describe "Cron" $ do
    it "always runs after a given delay" $ property $
      \(ArbitraryTime now) ->
        let now'       = CurrentTime now
            plusOneDay = now `addTime` (86400 :: Delay)
        in
          runAt (Cron daily) now' === Just (ScheduledTime $ replaceTime plusOneDay 0)

    it "produces a successor job" $
      pendingWith "No way to check equality through an existential type"
