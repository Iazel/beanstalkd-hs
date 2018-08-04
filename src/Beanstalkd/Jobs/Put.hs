{-# LANGUAGE OverloadedStrings #-}
module Beanstalkd.Jobs.Put where

import Beanstalkd.Common
import Beanstalkd.Internals.ToByteStringBuilder (conv)
import Beanstalkd.Internals.ParseResponse
import Beanstalkd.Internals.Parser (value, eol)
import Network.Socket.ByteString (recv, send)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString)
import Control.Applicative ((<|>), (<*))
import qualified Data.Attoparsec.ByteString.Char8 as P

data PutResponse
    = Inserted ID
    | Buried ID
    | JobTooBig
    | Draining
    | ExpectedCRLF

instance ParseResponse PutResponse where
    parser = (withId "INSERTED " Inserted)
        <|> (withId "BURIED " Buried)
        <|> (value "JOB_TOO_BIG\r\n" JobTooBig)
        <|> (value "DRAINING\r\n" Draining)
        <|> (value "EXPECTED_CRLF\r\n" ExpectedCRLF)
        where
            withId str constr =
                P.string str >> parseId constr <* eol
            parseId constr = do
                id <- P.many1 P.digit
                return $ constr (read id)

put :: Priority -> Delay -> TTR -> Job -> Conn -> IO (Response PutResponse)
put prio delay ttr job (Conn sock) = do
    send sock request
    response <- recv sock 1024
    return $ parse response
    where request = toStrict . toLazyByteString
            $  (conv prio)
            <> (conv delay) 
            <> (conv ttr)
            <> (jobLen job) 
            <> sep 
            <> (conv job)
            <> sep

puts = put (Priority 100) (Seconds 0) (TTR 60)
