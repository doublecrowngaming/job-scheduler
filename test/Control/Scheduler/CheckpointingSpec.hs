{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Control.Scheduler.CheckpointingSpec (spec) where

import           Control.Scheduler

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class  (MonadIO (..))
import           Control.Monad.Logger    (runNoLoggingT)
import           System.IO.Temp
import           Test.Hspec

spec :: Spec
spec = do
  describe "withLocalCheckpointing" $ do
    it "is ok with a non-existent checkpoint file" $
      withSystemTempDirectory "foo" $ \dirpath -> do
        let filepath = dirpath <> "/noexist"

        runNoLoggingT $
          runChronometerT $
            runScheduler @SingleThreaded (withLocalCheckpointing filepath $ schedule $ Job Immediately "foo")
        True `shouldBe` True

    it "is ok with a checkpoint file containing an empty list" $ do
      tmpfile <- emptySystemTempFile "foobar"
      writeFile tmpfile "[]"

      runNoLoggingT $
        runChronometerT $
          runScheduler @SingleThreaded (withLocalCheckpointing tmpfile $ schedule $ Job Immediately "foo")

      True `shouldBe` True

  describe "onColdStart" $ do
    it "runs when the checkpoint file doesn't exist" $ do
      mvar <- newEmptyMVar :: IO (MVar Int)

      withSystemTempDirectory "foo" $ \dirpath -> do
        let filepath = dirpath <> "/noexist"

        runNoLoggingT $
          runChronometerT $
            runScheduler @SingleThreaded $
              withLocalCheckpointing filepath $ do
                onColdStart (schedule $ Job Immediately "foo")

                react $ \case
                  "foo" -> liftIO $ putMVar mvar 3
                  _     -> liftIO $ putMVar mvar 0

      takeMVar mvar `shouldReturn` 3

    it "runs when the checkpoint file is an empty list" $ do
      mvar <- newEmptyMVar :: IO (MVar Int)

      withSystemTempDirectory "bar" $ \dirpath -> do
        let filepath = dirpath <> "/checkpoint"

        writeFile filepath "[]"

        runNoLoggingT $
          runChronometerT $
            runScheduler @SingleThreaded $
              withLocalCheckpointing filepath $ do
                onColdStart (schedule $ Job Immediately "foo")

                react $ \case
                  "foo" -> liftIO $ putMVar mvar 3
                  _     -> liftIO $ putMVar mvar 0

      takeMVar mvar `shouldReturn` 3

    it "doesn't run when the checkpoint file is non-empty" $ do
      mvar <- newEmptyMVar :: IO (MVar Int)

      withSystemTempDirectory "bar" $ \dirpath -> do
        let filepath = dirpath <> "/checkpoint"

        writeFile filepath "[]"

        runNoLoggingT $
          runChronometerT $ do
            runScheduler @SingleThreaded $
              withLocalCheckpointing filepath $
                schedule (Job Immediately "bar")

            runScheduler @SingleThreaded $
              withLocalCheckpointing filepath $ do
                onColdStart (schedule (Job Immediately "foo"))

                react $ \case
                  "foo" -> liftIO $ putMVar mvar 3
                  _     -> liftIO $ putMVar mvar 0

      takeMVar mvar `shouldReturn` 0
