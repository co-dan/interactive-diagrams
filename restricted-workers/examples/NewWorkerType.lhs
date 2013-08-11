> {-# LANGUAGE OverloadedStrings, TypeFamilies #-}
> module Main where

In this walkthrough we will learn how to make our worker types. Let's
consider the following motivational situation.

We want to have our worker perform some database operations based on
the commands it receives. The worker needs to connect to the database
on startup, based on some run-time information, store the connection
in memory and re-use this connection for every client that connects to
it.

We will use the 'sqlite-simple' library for the database access.

> import Control.Applicative     ((<$>), (<*>))
> import Control.Monad           (forever)
> import Database.SQLite.Simple
> import Data.Default    
> import Network                 (accept, Socket)
> import System.IO               (hGetLine, hClose)
> 
> import System.Restricted.Types        (LimitSettings)
> import System.Restricted.Worker
> import System.Restricted.Worker.Types

First of all, let's deal with abstractions for the underlying database.

Our database would have a table called 'accounts' with columns 'id',
'name' and 'age'. We can create such a database from a shell:

```
$ sqlite3 one.db "CREATE TABLE accounts (id INTEGER PRIMARY KEY, \
name text, age INTEGER);"
```

Some sqlite-simple boilerplate for representing our table in Haskell

> data Account = Account Int String Int
>              deriving (Show)
> 
> instance FromRow Account where
>     fromRow = Account <$> field <*> field <*> field
> 
> type Accounts = [Account]


Now we are going to roll out or worker type and corresponding
WorkerData instance.


> data DBWorker = DBWorker
> 
> instance WorkerData DBWorker where
>     type WData  DBWorker = Connection
>     type WMonad DBWorker = IO
     

Our worker would run in the IO monad and it would store 'Connection'
between sessions. This information is provided via type families in
the 'WorkerData' typeclass.

The next thing we should provide is the runner
function for our worker. Let's examine (a simplified) 'startWorker'
function:

```haskell
{-|
  Start a general type of worker.

  The pre-forking action is a monadic action that will be run prior to
  calling 'forkWorker'. It might be some initialization code, running the
  DB query, anything you want. The resulting 'WData' will be passed to
  the callback.
-}
startWorker :: (WorkerData w, MonadIO (WMonad w))
            => String         -- ^ Name
            -> FilePath       -- ^ Socket
            -> Maybe (IO Handle)  -- ^ Where to redirect stdout, stderr
            -> LimitSettings  -- ^ Restrictions
            -> WMonad w (WData w)  -- ^ Pre-forking action
            -> (WData w -> Socket -> IO ())  -- ^ Socket callback
            -> WMonad w (Worker w, RestartWorker (WMonad w) w)
```

Specified to 'DBWorker' the signature will be

```haskell
startWorker :: String              -- ^ Name
            -> FilePath            -- ^ Socket
            -> Maybe (IO Handle)   -- ^ Where to redirect stdout, stderr
            -> LimitSettings       -- ^ Restrictions
            -> IO Connection       -- ^ Pre-forking action
            -> (Connection -> Socket -> IO ())  -- ^ Socket callback
            -> IO (Worker DBWorker, RestartWorker IO DBWorker)
```

First of all, let's provide a callback for our type of worker:

> dbWorkerCallb :: (String -> Query) -> (Connection -> Socket -> IO ())
> dbWorkerCallb mkQuery conn sock = forever $ do
>     (hndl, _, _) <- accept sock
>     ln <- hGetLine hndl
>     r <- query_ conn (mkQuery ln) :: IO Accounts
>     mapM_ print r
>     putStrLn "--------------------------------------------------"
>     hClose hndl

Our callback (more of a callback generator) takes a function which
converts String of incoming data to a query and performs it. The
socket that we receive is a server socket which accepts connections,
that's why we need a 'forever' above.

All is left is to write a general runDbWorker function.

> runDbWorker :: String             -- ^ Name
>             -> LimitSettings      -- ^ Restrictions
>             -> IO Connection      -- ^ Action that gets the connection
>             -> (String -> Query)  -- ^ Query convertion function
>             -> IO (Worker DBWorker, RestartWorker IO DBWorker)
> runDbWorker name settings initConn mkQuery =
>     startWorker name path Nothing settings initConn callback          
>   where
>     path     = "/tmp/" ++ name ++ ".sock"
>     callback = dbWorkerCallb mkQuery


To test our newly crafted worker type let's make some dummy values.

> dummyInit :: IO Connection
> dummyInit = msg >> conn -- usually here we might want to consult a
> -- configuration file or whatnot to get the information during the
> -- run-time
>   where conn = open "one.db"
>         msg  = putStrLn "Welcome to dummybot 2000"

> dummyCb :: String -> Query
> dummyCb "all"    = "SELECT * FROM accounts;"
> dummyCb "adults" = "SELECT * FROM accounts WHERE (age >= 18);"


At once we can go ahead and run our worker

> main :: IO ()
> main = do
>     (dummyWorker, _) <- runDbWorker "dummy" def dummyInit dummyCb
>     _ <- getLine
>     killWorker dummyWorker
>     return ()

Sample output:

```
$ sqlite3 one.db "INSERT INTO accounts (name, age) VALUES ('Bob Dole', 14)"
$ sqlite3 one.db "INSERT INTO accounts (name, age) VALUES ('Dob Bole', 15)"
$ sqlite3 one.db "INSERT INTO accounts (name, age) VALUES ('Stephen Colbert',  18)"
$ sqlite3 one.db "INSERT INTO accounts (name, age) VALUES ('Jon Steward',  24)"
$ ./NewWorkerType
Welcome to dummybot 2000

```

In another terminal window:

```
$ echo all | nc -U /tmp/dummy.sock
$ echo adults | nc -U /tmp/dummy.sock
```

Back to the first window:

```
<...>
Welcome to dummybot 2000

Account 1 "Bob Dole" 14
Account 2 "Dob Bole" 15
Account 3 "Stephen Colbert" 18
Account 4 "Jon Steward" 24
--------------------------------------------------
Account 3 "Stephen Colbert" 18
Account 4 "Jon Steward" 24
--------------------------------------------------
```
