{-# LANGUAGE ScopedTypeVariables #-}

module Control.Scheduler.Task.AtSpec (spec) where

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
  describe "At" $ do
    it "runs at a given time" $ property $
      \(ArbitraryTime now, ArbitraryTime runtime) ->
        let scheduledTime = ScheduledTime runtime
            currentTime   = CurrentTime now
        in
          runAt (At scheduledTime ()) currentTime === Just scheduledTime

    it "produces no successor jobs" $ property $
      \(ArbitraryTime now, ArbitraryTime runtime) ->
        let scheduledTime = ScheduledTime runtime
            currentTime   = CurrentTime now
        in
          isNothing $ nextJob (At scheduledTime ()) currentTime

    it "applies routines against its contents" $ property $
      \(num :: Int, ArbitraryTime runtime) ->
        let scheduledTime = ScheduledTime runtime
        in
          apply (At scheduledTime num) (\x -> pure $ x + 1) === Identity (num + 1)
