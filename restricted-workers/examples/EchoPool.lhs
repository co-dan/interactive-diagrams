> {-# LANGUAGE OverloadedStrings #-}
> module Main where

In this example we'll take a look how to use 'System.Restricted.Worker.Pool' to create a
pool of EchoWokers. See also: 'EchoWorker.lhs'.

> import Control.Concurrent (forkIO, threadDelay)
> import Control.Monad      (forever, void)
> import qualified Data.ByteString as BS
> import Data.Default       (def)
> import Data.Monoid        ((<>))
> import System.IO          (Handle, stdin, hClose,
>                            hSetBuffering, BufferMode(..))
> 
> import System.Restricted.Worker
> import System.Restricted.Worker.Pool

This is just the 'echoHandler' function from 'EchoWorker.lhs':

> echoHandler :: Handle -> IO ()
> echoHandler h = do
>     hSetBuffering h LineBuffering
>     s <- BS.hGetLine h
>     BS.putStrLn $ "Got: " <> s
>     BS.hPutStrLn h s
>     hClose h

Let's examine the type of 'System.Restricted.Worker.Pool.mkPool':

```haskell
mkPool :: (Int -> IO (Worker IOWorker, RestartWorker IO IOWorker))
       -- ^ An action that creates a new worker. Takes a unique number as an argument
       -> Int
       -- ^ Maximum number of workers in the pool   
       -> Int
       -- ^ Restart rate (in seconds)
       -> IO (WorkersPool a)
```

Let's implement those parameters.
 
We allow a maximum of two workers being active concurrently

> maxWorkers :: Int
> maxWorkers = 2

Restart rate is how frequently we restart inactive workers. In this
case we restart inactive workers (workers not being currently used)
every 5 seconds. Normally you would specify a bigger number.

> restartRate :: Int
> restartRate = 5

Now we come to the more interesting part. Implementing an action that
creats a new worker. This is callback that gets called with a unique
worker id. No two live workers share the same id.

> newWorkerAct :: Int -> IO (Worker IOWorker, RestartWorker IO IOWorker)
> newWorkerAct uid = startIOWorker wname def wsock echoHandler
>   where wname = "Worker" ++ show uid
>         wsock = "/tmp/"  ++ show uid ++ ".sock"

We are ready to write code for initializing our pool:

> main :: IO ()
> main = do
>     hSetBuffering stdin NoBuffering
>     pool <- mkPool newWorkerAct maxWorkers restartRate
>     loop pool
 
Then we can write a simple control loop for out program.

When we recieve the "new" command from stdin, we take a worker from
the pool, wait for 8 seconds so the user can interact with it from
another shell and put the worker back.

> loop :: WorkersPool IOWorker -> IO ()
> loop pool = forever $ do
>     ln <- getLine
>     case ln of
>         "new" -> void $ forkIO $ do 
>             withWorker pool $ \(w,_) -> do
>                 putStrLn $ "Got worker          " ++
>                     show (workerName w)
>                 threadDelay 8000000
>                 putStrLn $ "Putting back worker " ++
>                     show (workerName w)
>         _     -> putStrLn "Dunno"


By running the program we can see that we can have up to two
simultaneously active workers:

```
$ ./EchoPool
new
Starting worker "Worker1"
Got worker          "Worker1"

Putting back worker "Worker1"
Starting worker "Worker1"

Starting worker "Worker1"

Starting worker "Worker1"

Starting worker "Worker1"

new
Got worker          "Worker1"
new
Starting worker "Worker2"
Got worker          "Worker2"

new
Putting back worker "Worker1"
<....>
```

