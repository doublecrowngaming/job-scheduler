{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Control.Scheduler.Time (
  Delay(..),
  Interval(..),
  ScheduledTime(..),
  CurrentTime(..),
  addTime,
  diffTime
) where

import           Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime,
                                  diffUTCTime)

newtype Delay         = Delay         NominalDiffTime deriving (Eq, Show, Num, Ord)
newtype Interval      = Interval      NominalDiffTime deriving (Eq, Show, Num, Ord)
newtype ScheduledTime = ScheduledTime UTCTime         deriving (Eq, Show, Ord)
newtype CurrentTime   = CurrentTime   UTCTime         deriving (Eq, Show)

addDelay :: UTCTime -> Delay -> UTCTime
addDelay time (Delay delay) = delay `addUTCTime` time

addInterval :: UTCTime -> Interval -> UTCTime
addInterval time (Interval interval) = realToFrac interval `addUTCTime` time

class AddableTime a b c where
  addTime :: a -> b -> c

instance AddableTime UTCTime Delay UTCTime where
  addTime = addDelay

instance AddableTime UTCTime Interval UTCTime where
  addTime = addInterval

instance AddableTime ScheduledTime Delay ScheduledTime where
  addTime (ScheduledTime time) = ScheduledTime . addTime time

instance AddableTime CurrentTime Delay CurrentTime where
  addTime (CurrentTime time) = CurrentTime . addTime time

instance AddableTime CurrentTime Delay ScheduledTime where
  addTime (CurrentTime time) = ScheduledTime . addTime time

instance AddableTime CurrentTime Interval ScheduledTime where
  addTime (CurrentTime time) = ScheduledTime . addTime time

class DiffableTime a b where
  diffTime :: a -> b -> Delay

instance DiffableTime UTCTime UTCTime where
  diffTime = (Delay .) . diffUTCTime

instance DiffableTime ScheduledTime CurrentTime where
  diffTime (ScheduledTime st) (CurrentTime ct) = Delay (st `diffUTCTime` ct)
