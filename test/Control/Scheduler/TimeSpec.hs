{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Scheduler.TimeSpec (spec) where

import           Control.Scheduler
import           Control.Scheduler.Time
import           Data.Time.Clock        (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX  (posixSecondsToUTCTime)

import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary UTCTime where
  arbitrary = posixSecondsToUTCTime . (realToFrac :: Double -> NominalDiffTime) <$> arbitrary

instance Arbitrary Interval where
  arbitrary = Interval . realToFrac <$> (arbitrary :: Gen Double)

spec :: Spec
spec = do
  describe "next" $
    it "is equivalent to the naïve implementation" $ property $
      \(reftime, ival, time) ->
        ival > 0 && time >= reftime ==> next (ReferenceTime reftime) ival time === referenceNext (ReferenceTime reftime) ival time
  describe "isAtOrAfter" $ do
    it "orders in time correctly" $
      (ScheduledTime $ read "1970-01-01 00:00:00") `isAtOrAfter` (CurrentTime $ read "1969-12-31 23:59:59")
    it "is true for the same time" $
      (ScheduledTime $ read "1970-01-01 00:00:00") `isAtOrAfter` (CurrentTime $ read "1970-01-01 00:00:00")
    it "orders in time correctly" $
      (CurrentTime $ read "1970-01-01 00:00:00") `isAtOrAfter` (ScheduledTime $ read "1969-12-31 23:59:59")
    it "is true for the same time" $
      (CurrentTime $ read "1970-01-01 00:00:00") `isAtOrAfter` (ScheduledTime $ read "1970-01-01 00:00:00")

  where
    referenceNext :: ReferenceTime -> Interval -> UTCTime -> UTCTime
    referenceNext (ReferenceTime baseTime) increment now =
      head $
        filter (> now) $
          iterate (`addInterval` increment) baseTime
