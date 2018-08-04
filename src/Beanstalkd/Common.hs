{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Beanstalkd.Common 
    ( module Beanstalkd.Common
    , module Beanstalkd.Error
    )
    where

import Prelude hiding (length)
import Data.Word (Word64, Word32)
import Data.ByteString (ByteString, length)
import Network.Socket (Socket)
import Beanstalkd.Error (Error)
import Data.ByteString.Builder (Builder, intDec, string7)
import Beanstalkd.Internals.ToByteStringBuilder (ToByteStringBuilder)

newtype ID = ID Word64
    deriving (Show, Read, ToByteStringBuilder)
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

type Response a = Either Error a

data GenericResponse
    = OK
    | NotFound

sep :: Builder
sep = string7 "\r\n"

jobLen :: Job -> Builder
jobLen (Job body) = intDec $ length body
