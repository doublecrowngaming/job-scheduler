{-# LANGUAGE ScopedTypeVariables #-}

module Control.Scheduler.Task.AfterSpec (spec) where

import           Control.Scheduler.Task
import           Control.Scheduler.Time (CurrentTime (..), Delay,
                                         ScheduledTime (..), addTime)
import           Data.Functor.Identity
import           Data.Maybe             (isNothing)
import           Data.Time.Clock        (UTCTime (..))

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
        runAt (After delay ()) (CurrentTime timestamp) === ScheduledTime (timestamp `addTime` delay)

    it "produces no successor jobs" $ property $
      \(ArbitraryTime timestamp, ArbitraryDelay delay) ->
        isNothing $ nextJob (After delay ()) (CurrentTime timestamp)

    it "applies routines against its contents" $ property $
      \(num :: Int, ArbitraryDelay delay) ->
        apply (After delay num) (\x -> pure $ x + 1) === Identity (num + 1)
