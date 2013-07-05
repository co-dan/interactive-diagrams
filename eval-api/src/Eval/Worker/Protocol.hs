{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Eval.Worker.Protocol where

import Control.Exception (IOException, throw, catch)
import Control.Applicative ((<$>))
import Data.ByteString (ByteString, hGetContents, hPutStr, hGetLine, putStr, hPut, hGet)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Serialize (encode, decode, Serialize)
import Data.Maybe (maybe)
import GHC.IO.Handle (Handle, hSetBuffering, BufferMode(..), hFlush)

import Eval (traceM, DecodeResult)  
import Eval.Worker.Types
import Eval.Worker.EvalCmd
import Display

sendData :: Serialize a => Handle -> a -> IO ByteString
sendData h d = sendData' h d
               `catch` \(e :: IOException) ->
               throw (HandleException e)

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
