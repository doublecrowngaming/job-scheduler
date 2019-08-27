{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Control.Scheduler.CheckpointingSpec (spec) where

import           Control.Scheduler

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class  (MonadIO (..))
import           System.IO.Temp
import           Test.Hspec

spec :: Spec
spec = do
  describe "withLocalCheckpointing" $ do
    it "is ok with a non-existent checkpoint file" $
      withSystemTempDirectory "foo" $ \dirpath -> do
        let filepath = dirpath <> "/noexist"

        runUninterruptable $
          runScheduler @SingleThreaded (withLocalCheckpointing filepath $ schedule Immediately "foo")
        True `shouldBe` True

    it "is ok with a checkpoint file containing an empty list" $ do
      tmpfile <- emptySystemTempFile "foobar"
      writeFile tmpfile "[]"

      runUninterruptable $
        runScheduler @SingleThreaded (withLocalCheckpointing tmpfile $ schedule Immediately "foo")

      True `shouldBe` True

  describe "onColdStart" $ do
    it "runs when the checkpoint file doesn't exist" $ do
      mvar <- newEmptyMVar :: IO (MVar Int)

      withSystemTempDirectory "foo" $ \dirpath -> do
        let filepath = dirpath <> "/noexist"

        runUninterruptable $
          runScheduler @SingleThreaded $
            withLocalCheckpointing filepath $ do
              onColdStart (schedule Immediately "foo")

              react $ \case
                "foo" -> liftIO $ putMVar mvar 3
                _     -> liftIO $ putMVar mvar 0

      takeMVar mvar `shouldReturn` 3

    it "runs when the checkpoint file is an empty list" $ do
      mvar <- newEmptyMVar :: IO (MVar Int)

      withSystemTempDirectory "bar" $ \dirpath -> do
        let filepath = dirpath <> "/checkpoint"

        writeFile filepath "[]"

        runUninterruptable $
          runScheduler @SingleThreaded $
            withLocalCheckpointing filepath $ do
              onColdStart (schedule Immediately "foo")

              react $ \case
                "foo" -> liftIO $ putMVar mvar 3
                _     -> liftIO $ putMVar mvar 0

      takeMVar mvar `shouldReturn` 3

    it "doesn't run when the checkpoint file is non-empty" $ do
      mvar <- newEmptyMVar :: IO (MVar Int)

      withSystemTempDirectory "bar" $ \dirpath -> do
        let filepath = dirpath <> "/checkpoint"

        writeFile filepath "[]"

        runUninterruptable $ do
          runScheduler @SingleThreaded $
            withLocalCheckpointing filepath $
              schedule Immediately "bar"

          runScheduler @SingleThreaded $
            withLocalCheckpointing filepath $ do
              onColdStart (schedule Immediately "foo")

              react $ \case
                "foo" -> liftIO $ putMVar mvar 3
                _     -> liftIO $ putMVar mvar 0

      takeMVar mvar `shouldReturn` 0