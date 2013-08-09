> {-# LANGUAGE OverloadedStrings #-}
> module Main where

In this example we will take a look at 'IOWorker' and how to use it in
your programs. We will create a simple worker that would echo back to
us whatever we sent it.

Let's start by importing some libraries

> import           Control.Concurrent    (threadDelay)
> import           Control.Monad         (when)
> import qualified Data.ByteString       as BS
> import           Data.Default          (def)
> import           Data.Monoid           ((<>))
> import           System.IO             (BufferMode (..), Handle, hClose,
>                                        hSetBuffering, stdin, stdout)
> import           Worker

Let's take a look at 'startIOWorker' function:

```haskell
startIOWorker :: String              -- ^ Name
              -> LimitSettings       -- ^ Restrictions
              -> FilePath            -- ^ UNIX socket
              -> (Handle -> IO ())   -- ^ Callback
              -> IO (Worker IOWorker, RestartWorker IO IOWorker)
```

it takes a worker name, a UNIX socket filepath, callback. It returns
the worker itself together with the restarting function.

Firstly, let's implement our callback:

> echoHandle :: Handle -> IO ()
> echoHandle h = do
>     hSetBuffering h LineBuffering
>     s <- BS.hGetLine h
>     BS.putStr $ "Got: " <> s <> "\n"
>     BS.hPutStrLn h s
>     hClose h

we are not doing anything fancy here, just getting one line of text,
printing it to stdout and sending it back.

Now we are going to write the main loop function that would take care
of restarting the worker:

> loop :: (Worker IOWorker, RestartWorker IO IOWorker) -> IO ()
> loop (w, restart) = do
>     putStrLn "Press <ENTER> to restart the worker"
>     _ <- getChar
>     w' <- restart w
>     loop (w', restart)
 

and a main function:

> main :: IO ()
> main = do
>     hSetBuffering stdin NoBuffering
>     loop =<< startIOWorker "Testworker" def "/tmp/1.sock" echoHandle
>     return ()

We can test it:

```
(In one shell)
$ ./EchoWorker
tarting worker "Testworker"
Press <ENTER> to restart the worker

(In another shell)
$ echo -n whatsup | nc -U /tmp/1.sock
whatsup
$ echo -n test | nc -U /tmp/1.sock
test
```

Now in the first window we can see the following:

```
Starting worker "Testworker"
Press <ENTER> to restart the worker

Got: whatsup
Got: test
```

We can press enter to restart the worker:

```
Starting worker "Testworker"
Press <ENTER> to restart the worker

Got: whatsup
Got: test

Starting worker "Testworker"
Press <ENTER> to restart the worker

```
