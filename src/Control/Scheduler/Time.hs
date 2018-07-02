{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Scheduler.Time (
  Delay(..),
  Interval(..),
  ReferenceTime(..),
  addDelay,
  addInterval,
  diffTime,
  subtractDelay,
  next
) where

import           Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime,
                                  diffUTCTime)

newtype Delay         = Delay         NominalDiffTime deriving (Eq, Show, Num, Ord)
newtype Interval      = Interval      NominalDiffTime deriving (Eq, Show, Num, Ord)
newtype ReferenceTime = ReferenceTime UTCTime         deriving (Eq, Show)

addDelay :: UTCTime -> Delay -> UTCTime
addDelay time (Delay delay) = delay `addUTCTime` time

subtractDelay :: UTCTime -> Delay -> UTCTime
subtractDelay time (Delay delay) = delay `addUTCTime` time

addInterval :: UTCTime -> Interval -> UTCTime
addInterval time (Interval interval) = realToFrac interval `addUTCTime` time

diffTime :: UTCTime -> UTCTime -> Delay
diffTime = (Delay .) . diffUTCTime

next :: ReferenceTime -> Interval -> UTCTime -> UTCTime
next (ReferenceTime baseTime) (Interval increment) now =
  totalIncrement `addUTCTime` baseTime
  where
    timeSinceReference      = now `diffUTCTime` baseTime
    intervalsSinceReference = floor (timeSinceReference / increment) :: Integer
    intervalsForNext        = intervalsSinceReference + 1
    totalIncrement          = increment * fromIntegral intervalsForNext :: NominalDiffTime
