{-# LANGUAGE ScopedTypeVariables #-}

module Control.Scheduler.Schedule.EverySpec (spec) where

import           Control.Scheduler.Schedule
import           Control.Scheduler.Time     (CurrentTime (..), Delay, Interval,
                                             ScheduledTime (..), addTime)
import           Data.Functor.Identity
import           Data.Maybe                 (isNothing)
import           Data.Time.Clock            (UTCTime (..))

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
  describe "Every" $ do
    it "always runs after a given delay" $ property $
      \(ArbitraryTime now, ArbitraryInterval interval, ArbitraryDelay delay) ->
        let now' = CurrentTime now
        in
          runAt (Every delay interval) now' === Just (ScheduledTime (now `addTime` delay))

    it "produces a successor job" $
      pendingWith "No way to check equality through an existential type"
