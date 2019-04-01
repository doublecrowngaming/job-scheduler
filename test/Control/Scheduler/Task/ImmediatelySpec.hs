{-# LANGUAGE ScopedTypeVariables #-}

module Control.Scheduler.Task.ImmediatelySpec (spec) where

import           Control.Scheduler.Task
import           Control.Scheduler.Time (CurrentTime (..), ScheduledTime (..))
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

spec :: Spec
spec =
  describe "Immediately" $ do
    it "always wants to run without delay" $ property $
      \(ArbitraryTime timestamp) -> runAt (Immediately ()) (CurrentTime timestamp) === Just (ScheduledTime timestamp)

    it "produces no successor jobs" $ property $
      \(ArbitraryTime timestamp) -> isNothing $ nextJob (Immediately ()) (CurrentTime timestamp)

    it "applies routines against its contents" $ property $
      \(num :: Int) -> apply (Immediately num) (\x -> pure $ x + 1) === Identity (num + 1)
