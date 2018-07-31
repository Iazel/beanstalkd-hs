{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Beanstalkd.Common where

import Prelude hiding (length)
import Data.Word (Word32)
import Data.ByteString (ByteString, length)
import Network.Socket (Socket)
import Data.ByteString.Builder (Builder, intDec, word32Dec, string7, byteString)

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

instance ParseResponse GenericResponse where
    parse msg = case msg of
        "BURIED\r\n"    -> OK
        "TOUCHED\r\n"   -> OK
        "DELETED\r\n"   -> OK
        "NOT_FOUND\r\n" -> NotFound
        otherwise       -> Error $ parse msg

sep = string7 "\r\n"
jobLen :: Job -> Builder
jobLen (Job body) = intDec $ length body
