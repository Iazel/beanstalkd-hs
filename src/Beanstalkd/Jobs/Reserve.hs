{-# LANGUAGE OverloadedStrings #-}
module Beanstalkd.Jobs.Reserve where

import Prelude hiding (id)
import Data.ByteString
import Network.Socket.ByteString (recv, send)
import Beanstalkd.Common
import Control.Applicative ((<|>), (<*), (*>))
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
        <|> parserReserved

reserve :: Conn -> IO (Response ReserveResponse)
reserve (Conn sock) = (send sock "reserve\r\n") >> recv sock 2048 >>= parseResponse
    where
        parseResponse msg = parseWith (recv sock 4096) msg

parserReserved :: P.Parser ReserveResponse
parserReserved = P.string "RESERVED " *> (Reserved <$> parseId <*> parseJob) <* eol
    where 
        parseJob = parseLen >>= mkJob
        parseLen = P.char ' ' *> P.decimal <* eol
        mkJob len = P.take len >>= (return . Job)
