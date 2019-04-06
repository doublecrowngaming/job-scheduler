{-# LANGUAGE ScopedTypeVariables #-}

module Control.Scheduler.Schedule.AfterSpec (spec) where

import           Control.Scheduler.Schedule
import           Control.Scheduler.Time     (CurrentTime (..), Delay,
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

spec :: Spec
spec =
  describe "After" $ do
    it "always runs after a given delay" $ property $
      \(ArbitraryTime timestamp, ArbitraryDelay delay) ->
        runAt (After delay) (CurrentTime timestamp) === Just (ScheduledTime (timestamp `addTime` delay))

    it "produces no successor jobs" $ property $
      \(ArbitraryTime timestamp, ArbitraryDelay delay) ->
        isNothing $ nextJob (After delay) (CurrentTime timestamp)
