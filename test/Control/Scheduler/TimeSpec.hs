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
spec =
  describe "next" $
    it "is equivalent to the naïve implementation" $ property $
      \(reftime, ival, time) ->
        ival > 0 && time >= reftime ==> next (ReferenceTime reftime) ival time === referenceNext (ReferenceTime reftime) ival time

  where
    referenceNext :: ReferenceTime -> Interval -> UTCTime -> UTCTime
    referenceNext (ReferenceTime baseTime) increment now =
      head $
        filter (> now) $
          iterate (`addInterval` increment) baseTime
