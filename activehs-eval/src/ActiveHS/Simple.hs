-- From ActiveHS (c) Péter Diviánszky

{-# LANGUAGE RankNTypes, ExistentialQuantification, ScopedTypeVariables, PatternGuards, FlexibleContexts #-}

module ActiveHS.Simple
    ( IError(..), Task (..), TaskChan
    , startGHCiServer
    , restartGHCiServer
    , interpret
-- , typeOf
    ) where

-- GHC
--import Hint.Base
--import Hint.Compat
--import Hint.Conversions
import Language.Haskell.Interpreter hiding (interpret, typeOf)

import Control.Concurrent (forkIO, ThreadId(..))
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar, tryPutMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException, AsyncException, catch, catches)
import Control.Monad (when, forever)
import Control.Monad.Error (MonadError, catchError)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Prelude hiding (catch)
import Debug.Trace
-------------------------

type IError a = Either InterpreterError a

data Task
    = forall a. Task FilePath (MVar (IError a)) (Interpreter a)

newtype TaskChan
    = TC (Chan (Maybe Task))

---------------

startGHCiServer :: [String] -> (String -> IO ()) -> (String -> IO ()) -> IO (TaskChan,ThreadId)
startGHCiServer paths logError logMsg = do
    ch <- newChan
    ref <- newIORef (const $ return ())

    tid <- forkIO $ forever $ do
        logMsg "start interpreter"
        e <- runInterpreter (handleTask ref ch Nothing)
              -- `catch` \(e :: AsyncException) -> error "Async error"
              -- `catch` \(e :: SomeException) -> do
              --   let err = UnknownError ("GHCi server died:" ++ show e)
              --   doPut <- readIORef ref
              --   doPut err
              --   return $ Left err
        case e of
            Left e -> logError $ "stop interpreter: " ++ show e
            Right _ -> return ()

    return (TC ch, tid)

  where
    handleTask :: IORef (InterpreterError -> IO ())
               -> Chan (Maybe Task) -> Maybe FilePath -> Interpreter ()
    handleTask ref ch oldFn = do
        task <- lift $ readChan ch
        case task of
            Just task@(Task fn repVar m) -> do
              lift $ writeIORef ref (\x -> tryPutMVar repVar (Left x) >> return ())
              handleTask_ ref ch oldFn task
            Nothing ->
              liftIO $ logError "interpreter stopped intentionally"

    handleTask_ ref ch oldFn (Task fn repVar m) = do
        (cont, res) <- do
            when (oldFn /= Just fn) $ do
                reset
                set [searchPath := paths]
                loadModules [fn]
                setTopLevelModules [fn]
            
            x <- m
            return (True, Right x)

          `catchError` \er ->
            return (not $ fatal er, Left er)

        lift $ putMVar repVar res
        when cont $ handleTask ref ch $ case res of
            Right _ -> Just fn
            Left _ -> Nothing


restartGHCiServer :: TaskChan -> IO ()
restartGHCiServer (TC ch) = writeChan ch Nothing

interpret :: TaskChan -> FilePath -> Interpreter a -> IO (IError a)
interpret (TC ch) fn m = do
    rep <- newEmptyMVar
    writeChan ch $ Just $ Task fn rep m
    takeMVar rep

{-
typeOf :: String -> Interpreter String
typeOf xs = do
--failOnParseError parseExpr xs
ty <- runGhc1 exprType xs
fromGhcRep $ fromJust ty
-}

fatal :: InterpreterError -> Bool
fatal (WontCompile _) = False
fatal (NotAllowed _) = False
fatal _ = True
