{-# LANGUAGE OverloadedStrings #-}
module Beanstalkd.Jobs.Release where

import Prelude hiding (id)
import Beanstalkd.Common
import Beanstalkd.Internals.ParseResponse
import Beanstalkd.Internals.ToByteStringBuilder (conv)
import Beanstalkd.Internals.Parser (value, notFound)
import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString, string7, char7)
import Network.Socket.ByteString (recv, send)

data ReleaseResponse = Released | Buried | NotFound deriving Show

instance ParseResponse ReleaseResponse where
    parser = value "RELEASED\r\n" Released
        <|> value "BURIED\r\n" Buried
        <|> notFound NotFound

release :: Priority -> Delay -> ID -> Conn -> IO (Response ReleaseResponse)
release prio delay id (Conn sock) = 
    send sock request >> recv sock 16 >>= return . parse
    where 
        request = toStrict $ toLazyByteString $
            (string7 "release ")
            <> conv id 
            <> char7 ' '
            <> conv prio 
            <> char7 ' '
            <> conv delay 
            <> sep

-- Short version of release with sensible defautls
rels :: ID -> Conn -> IO (Response ReleaseResponse)
rels = release (Priority 100) (Seconds 10)
