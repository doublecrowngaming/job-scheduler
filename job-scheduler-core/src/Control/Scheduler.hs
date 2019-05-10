{-|
Module      : Control.Scheduler
Description : A simple job scheduler
Copyright   : (c) Binary Sunrise Inc. 2017 - 2018
              (c) Double Crown Gaming Co. 2019
License     : BSD
Maintainer  : jesse.kempf@doublecrown.co
Stability   : experimental

This libary lets you build applications that run various tasks on different schedules.
You can think of it like a programmable version of the Unix @at(1)@ and @cron(8)@.

Here's a usage example:

> data JobEnum = JFoo | JBar | JForkBomb
>
> runScheduler @SingleThreaded $ do
>  schedule (Every 15 30) JFoo
>  schedule (After 60) JBar
>  schedule (After 120) JForkBomb
>
>  react $ \case
>    JFoo      -> liftIO (putStrLn "FOO!")
>    JBar      -> liftIO (putStrLn "BAR!")
>    JForkBomb -> do
>      schedule Immediately JForkBomb
>      schedule Immediately JForkBomb

After 15 seconds from program start, the program will print @FOO!@ every 30 seconds.
After 60 seconds from program start, the program will print @BAR!@ once.
After 120 seconds from program start, the program will schedule two jobs for immediate execution that themselves schedule two jobs, and so on.

As you can see, jobs can run any sort of action -- including scheduling other jobs.
Jobs are represented as data which is presented to a user-provided handler.
-}

module Control.Scheduler (
  module Control.Scheduler.Class,
  module Control.Scheduler.Schedule,
  module Control.Scheduler.Time,
  module Control.Scheduler.Type,
  module Control.Scheduler.Runner.SingleThreaded
) where


import           Control.Scheduler.Class                 (MonadScheduler, react,
                                                          schedule)
import           Control.Scheduler.Runner.SingleThreaded
import           Control.Scheduler.Schedule              (Schedule (..))
import           Control.Scheduler.Time                  (Delay (..),
                                                          Interval (..),
                                                          ScheduledTime (..))
import           Control.Scheduler.Type                  (RunnableScheduler (..),
                                                          Scheduler,
                                                          unScheduler)
