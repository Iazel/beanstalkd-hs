{-# LANGUAGE OverloadedStrings #-}
module Beanstalkd.Jobs.Put where

import Prelude hiding (id)
import Beanstalkd.Common
import Beanstalkd.Internals.ToByteStringBuilder (conv)
import Beanstalkd.Internals.ParseResponse
import Beanstalkd.Internals.Parser (value, eol)
import Network.Socket.ByteString (recv, send)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString, char7, string7)
import Control.Applicative ((<|>), (<*))
import qualified Data.Attoparsec.ByteString.Char8 as P

data PutResponse
    = Inserted ID
    | Buried ID
    | JobTooBig
    | Draining
    | ExpectedCRLF
    deriving Show

instance ParseResponse PutResponse where
    parser = (withId "INSERTED " Inserted)
        <|> (withId "BURIED " Buried)
        <|> (value "JOB_TOO_BIG\r\n" JobTooBig)
        <|> (value "DRAINING\r\n" Draining)
        <|> (value "EXPECTED_CRLF\r\n" ExpectedCRLF)
        where
            withId str constr =
                P.string str >> (parseId >>= return . constr) <* eol

put :: Priority -> Delay -> TTR -> Job -> Conn -> IO (Response PutResponse)
put prio delay ttr job (Conn sock) = do
    _ <- send sock request
    response <- recv sock 1024
    return $ parse response
    where request = toStrict . toLazyByteString
            $  string7 "put "
            <> conv prio
            <> char7 ' '
            <> (conv delay) 
            <> char7 ' '
            <> (conv ttr)
            <> char7 ' '
            <> (jobLen job) 
            <> sep 
            <> (conv job)
            <> sep

puts :: Job -> Conn -> IO (Response PutResponse)
puts = put (Priority 100) (Seconds 0) (TTR 60)
