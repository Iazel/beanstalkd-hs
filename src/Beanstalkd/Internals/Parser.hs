{-# LANGUAGE OverloadedStrings #-}
module Beanstalkd.Internals.Parser where

import Data.ByteString (ByteString)
import qualified Data.Attoparsec.ByteString.Char8 as P

value :: ByteString -> a -> P.Parser a
value match val = P.string match >> return val

notFound :: a -> P.Parser a
notFound val = value "NOT_FOUND\r\n" val

eol :: P.Parser ByteString
eol = P.string "\r\n"
