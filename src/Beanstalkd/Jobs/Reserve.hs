{-# LANGUAGE OverloadedStrings #-}
module Beanstalkd.Jobs.Reserve where

import Prelude hiding (id)
import Network.Socket.ByteString (recv, send)
import Beanstalkd.Common
import Beanstalkd.Internals.ToByteStringBuilder (conv)
import Control.Applicative ((<|>), (<*), (*>))
import Data.ByteString (ByteString)
import Data.ByteString.Builder (string7)
import Beanstalkd.Internals.ParseResponse
import Beanstalkd.Internals.Parser (value, eol)
import qualified Data.Attoparsec.ByteString.Char8 as P

data ReserveResponse
    = TimedOut
    | DeadlineSoon
    | Reserved ID Job
    deriving Show

instance ParseResponse ReserveResponse where
    parser = value "TIMED_OUT\r\n" TimedOut
        <|> value "DEADLINE_SOON\r\n" DeadlineSoon
        <|> P.string "RESERVED " *> (Reserved <$> parseId <*> parseJob) <* eol
        where
            parseJob = parseLen >>= mkJob
            parseLen = P.char ' ' *> P.decimal <* eol
            mkJob len = Job <$> P.take len

reserve :: Conn -> IO (Response ReserveResponse)
reserve = doReserve "reserve\r\n"

reserveWithTimeout :: Seconds -> Conn -> IO (Response ReserveResponse)
reserveWithTimeout to = doReserve request
    where request = toBStr $ (string7 "reserve-with-timeout ") <> conv to <> sep

doReserve :: ByteString -> Conn -> IO (Response ReserveResponse)
doReserve cmd (Conn sock) = 
    (send sock cmd) >> recv sock 2048 >>= parseResponse
    where
        parseResponse msg = parseWith (recv sock 4096) msg
