{-# LANGUAGE OverloadedStrings #-}
module Beanstalkd.Jobs.Delete where

import Prelude hiding (id)
import Beanstalkd.Common
import Beanstalkd.Internals.ToByteStringBuilder (conv)
import Beanstalkd.Internals.ParseResponse
import Beanstalkd.Internals.Parser (value, notFound)
import Control.Applicative ((<|>))
import Network.Socket.ByteString (recv, send)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString)

data DeleteResponse = Deleted | NotFound
    deriving Show

instance ParseResponse DeleteResponse where
    parser = value "DELETED\r\n" Deleted
          <|> notFound NotFound

delete :: ID -> Conn -> IO (Response DeleteResponse)
delete id (Conn sock) = send sock request >> recv sock 16 >>= return . parse
    where request = toStrict $ toLazyByteString $ "delete " <> conv id <> sep
