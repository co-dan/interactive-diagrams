{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A simple protocol for sending serializable data over handles
--
-- Please note that this is a very simple implementation that works
-- fine for most of that data, however, the size of the data you might
-- send at one go is limited to MAX_WORD32 bytes. We use 'cereal' for
-- serialization.
module System.Restricted.Worker.Protocol
    (
      sendData
    , getData
    , getDataSafe
    , DecodeResult
    , ProtocolException(..)
    ) where

import           Control.Applicative            ((<$>))
import           Control.Exception              (IOException, catch, throw)
import           Control.Monad.Trans            (lift)
import           Control.Monad.Trans.Either     (hoistEither, left, runEitherT)
import           Data.ByteString                (ByteString, hGet, hPut)
import qualified Data.ByteString                as BS
import           Data.Serialize                 (Serialize, decode, encode)
import           Data.Word                      (Word32)
import           GHC.IO.Handle                  (Handle, hFlush)

import           System.Restricted.Worker.Types

-- | Result of the deserialization
type DecodeResult a = Either String a

-- | Send some serialiazable data over a handle.
-- Returns 'ByteString' representing the encoded data. May throw
-- 'ProtocolException'
sendData :: Serialize a => Handle -> a -> IO ByteString
sendData h d = sendData' h d
               `catch` \(e :: IOException) ->
               throw (HandleException e)

-- | Read the data from a handle and deserialize it.
-- May throw 'ProtocolException'
getData :: Serialize a => Handle -> IO a
getData h = getData' h
            `catch` \(e :: IOException) ->
            throw (HandleException e)


sendData' :: Serialize a => Handle -> a -> IO ByteString
sendData' hndl datum = do
    let encoded = encode datum
    let len     = (fromIntegral . BS.length $ encoded) :: Word32
    hPut hndl (encode len)
    hFlush hndl
    hPut hndl encoded
    hFlush hndl
    return encoded

getData' :: Serialize a => Handle -> IO a
getData' hndl = do
    lenD :: DecodeResult Word32 <- decode <$> hGet hndl 4
    let len = case lenD of
            Right i -> fromIntegral i
            Left str -> throw (ConversionException $ "length\n" ++ str)
    res <- decode <$> hGet hndl len
    case res of
        Left str -> throw (ConversionException $ "Deserialization error:\n" ++ str)
        Right x  -> return x

-- | Safe version of 'getData' that doesn't throw 'ProtocolException'
getDataSafe :: Serialize a => Handle -> IO (DecodeResult a)
getDataSafe hndl = runEitherT $ do
    lenD :: DecodeResult Word32 <- decode <$> lift (hGet hndl 4)
    case lenD of
        Left str -> left $ "Conversion error while reading length: " ++ str
        Right len ->
            hoistEither =<< decode <$> (lift (hGet hndl (fromIntegral len)))

