{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | A simple protocol for sending serializable data over handles
module Eval.Worker.Protocol (sendData, getData) where

import Control.Applicative ((<$>))
import Control.Exception (IOException, throw, catch)
import Data.ByteString (ByteString, hGetLine, hPut, hGet)
import qualified Data.ByteString as BS
import Data.Serialize (encode, decode, Serialize)
import GHC.IO.Handle (Handle, hFlush)

import Eval (traceM, DecodeResult)  
import Eval.Worker.Types

-- | Send some serialiazable data over a handle.
-- Returns 'ByteString' representing the encoded data
sendData :: Serialize a => Handle -> a -> IO ByteString
sendData h d = sendData' h d
               `catch` \(e :: IOException) ->
               throw (HandleException e)

-- | Read the data from a handle and deserialize it.               
getData :: Serialize a => Handle -> IO a
getData h = getData' h
            `catch` \(e :: IOException) ->
            throw (HandleException e)
               
sendData' :: Serialize a => Handle -> a -> IO ByteString
sendData' hndl datum = do
  let encoded = encode datum
  let len     = BS.length encoded
  -- we don't know what is the length of output
  -- but maybe we should use 'BS.singleton' and 'Char' for length?      
  hPut hndl (encode len)
  hPut hndl "\n"
  hFlush hndl
  hPut hndl encoded
  hPut hndl "\n"
  hFlush hndl
  return encoded

getData' :: Serialize a => Handle -> IO a
getData' hndl = do
  lenS :: DecodeResult Int <- decode <$> hGetLine hndl
  let len = case lenS of
        Right i -> i
        Left str -> throw (ConversionException $ "length\n" ++ str)
  res <- decode <$> hGet hndl len
  case res of
    Left str -> throw (ConversionException $ "Deserialization error:\n" ++ str)
    Right x  -> return x
