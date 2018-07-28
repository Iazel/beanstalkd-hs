{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Types where

import Data.Word (Word32)
import Data.ByteString (ByteString)
import Network.Socket (Socket)
import Data.ByteString.Builder (Builder)

newtype ID = ID Word32
    deriving (Show, ToByteStringBuilder)
newtype Priority = Priority Word32
    deriving (Show, ToByteStringBuilder)
newtype Seconds = Seconds Word32
    deriving (Show, ToByteStringBuilder)
newtype TTR = TTR Word32
    deriving (Show, ToByteStringBuilder)
newtype Job = Job ByteString
    deriving (Show, ToByteStringBuilder)
newtype Tube = Tube ByteString
    deriving (Show, ToByteStringBuilder)

newtype Conn = Conn Socket

type Delay = Seconds
type Count = Word32
type Amount = Word32

data GenericResponse
	= OK
	| NotFound
    | Error Error

data Error
	= OutOfMemory
	| InternalError
	| BadFormat
	| UnknownCommand
    | UnknownResponse ByteString

class ToByteStringBuilder a where
    conv :: a -> Builder

instance ToByteStringBuilder ByteString where
    conv = byteString

instance ToByteStringBuilder Word32 where
    conv = word32Dec

sep = string7 "\r\n"
jobLen :: Job -> Builder
jobLen (Job body) = intDec $ length body

class ParseReponse a where
    parse :: ByteString -> a

instance ParseReponse Error where
    parse msg = case msg of
        "OUT_OF_MEMORY\r\n"   -> OutOfMemory
        "INTERNAL_ERROR\r\n"  -> InternalError
        "BAD_FORMAT\r\n"      -> BadFormat
        "UNKNOWN_COMMAND\r\n" -> UnknownCommand
        otherwise             -> UnknownResponse msg

instance ParseResponse GenericResponse where
    parse msg = case msg of
        "BURIED\r\n"    -> OK
        "TOUCHED\r\n"   -> OK
        "DELETED\r\n"   -> OK
        "NOT_FOUND\r\n" -> NotFound
        otherwise       -> Error $ parse msg
