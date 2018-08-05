{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Beanstalkd.Common 
    ( module Beanstalkd.Common
    , module Beanstalkd.Error
    )
    where

import Prelude hiding (id, length)
import Data.Word (Word64, Word32)
import Data.ByteString (ByteString, length)
import Network.Socket (Socket)
import Beanstalkd.Error (Error)
import Data.ByteString.Builder (Builder, intDec, string7)
import Beanstalkd.Internals.ToByteStringBuilder (ToByteStringBuilder)
import Data.Attoparsec.ByteString.Char8 (Parser, many1, digit)

newtype ID = ID Word64
    deriving (Show, ToByteStringBuilder)

parseId :: Parser ID
parseId = do
    id <- many1 digit
    return $ ID (read id)

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

sep :: Builder
sep = string7 "\r\n"

jobLen :: Job -> Builder
jobLen (Job body) = intDec $ length body
