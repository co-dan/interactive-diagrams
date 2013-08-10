> {-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
> module Main where

We already know how to define simple workers that communicate with the
client using text. But sometimes it's necessary to transfer more
complex data to the worker. We provide a simple way of tackling this
problem using the 'System.Restricted.Worker.Protocol' module.

> import Control.Monad         (forM_, (<=<))
> import Control.Concurrent    (threadDelay)
> import Data.Default          (def)
> import Data.Serialize        (Serialize)
> import GHC.Generics          (Generic)
> import System.IO             (BufferMode (..), Handle, hClose,
>                               hSetBuffering, stdin)
>     
> import System.Restricted.Worker
> import System.Restricted.Worker.Protocol

Imaging we want our worker to receive commands in the form of the
following data structure:

> data CommandW = AddNumbers Int Int
>               | Dance
>               | Echo String
>               | Bye
>               deriving (Generic)
> 
> instance Serialize CommandW where
>                        
> evalCommand :: CommandW -> IO ()
> evalCommand (AddNumbers a b) = print $ a + b
> evalCommand Dance            = putStrLn "♪ *dum* *dum* ♫"
> evalCommand (Echo s)         = putStrLn s
> evalCommand Bye              = return ()

If we want to be able to use 'Worker.Protocol' with our data-types we
must define 'Serialize' instances for them. 'Serialize' is a typeclass
from the 'cereal' package and usually you can use the default
implementation (as you have seen above) as long as you can derive
'Generic' for your data-types.

After we are done with our type we can go on defining our handler.
'System.Restricted.Worker.Protocol' provides us with useful functions
for sending and receiving data:

```haskell
-- | Send some serialiazable data over a handle.
-- Returns 'ByteString' representing the encoded data. May throw
-- 'ProtocolException'
sendData :: Serialize a => Handle -> a -> IO ByteString

-- | Read the data from a handle and deserialize it.
-- May throw 'ProtocolException'
getData :: Serialize a => Handle -> IO a
```

Let's try to implement our handler in terms of those functions.

> cmdHandler :: Handle -> IO ()
> cmdHandler hndl = loop =<< getData hndl
>   where
>     loop Bye = hClose hndl
>     loop cmd = evalCommand cmd >> getData hndl >>= loop

Our worker evaluates incoming commands until it hits 'Bye'.

Now let's implement the part of our program that would send commands to the worker:

> sendCommands :: Worker IOWorker -> [CommandW] -> IO ()
> sendCommands worker cmds = do
>     hndl <- connectToWorker worker
>     forM_ cmds (sendData hndl)
>     sendData hndl Bye
>     return ()

In the code above we are using 'connectToWorker' function from
'Worker.Internal', which simply connects to the active worker and
returns a handle for communication.

In the main function we delay for 80000 before killing the worker (or exiting) to make sure that the worker process has time to print its output.

> main :: IO ()
> main = do
>     hSetBuffering stdin NoBuffering
>     (worker,_) <- startIOWorker "Command Evaluator" def
>                       "/tmp/commandme.sock" cmdHandler
>     sendCommands worker [Dance, Dance, AddNumbers 1 2, Echo "Hi", Dance]
>     threadDelay 80000
>     killWorker worker
>     return ()

Sample output:

```
$ runhaskell CommandEvalProtocol.lhs
Starting worker "Command Evaluator"

♪ *dum* *dum* ♫
♪ *dum* *dum* ♫
3
Hi
♪ *dum* *dum* ♫
```
