{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Scheduler.Logging (
  Logger(..),
  nullLogger,
  HasLogger(..),
  LogLevel(..),
  LogSource(..),
  slog
) where

import           Control.Monad.Trans
import           Data.ByteString.Builder (Builder, byteString)
import           Data.String             (IsString)
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8)

data LogLevel     = TRACE | DEBUG | INFO | ERROR deriving (Eq, Ord, Show)
newtype LogSource = LogSource Text deriving (Eq, Show, IsString)

newtype Logger = Logger (LogLevel -> LogSource -> Builder -> IO ())

nullLogger :: Logger
nullLogger = Logger (\_ _ _ -> pure ())

class HasLogger m where
  getLogger :: m Logger

instance {-# OVERLAPPABLE #-} (HasLogger m, MonadTrans t, Monad m, Monad (t m)) => HasLogger (t m) where
  getLogger = lift getLogger

slog :: (MonadIO m, HasLogger m) => LogLevel -> LogSource -> Text -> m ()
slog lvl src msg = do
  (Logger logger) <- getLogger

  liftIO $ logger lvl src bmsg
  where
    bmsg = byteString . encodeUtf8 $ msg
