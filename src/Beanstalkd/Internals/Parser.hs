{-# LANGUAGE OverloadedStrings #-}
module Beanstalkd.Internals.Parser where

import Data.ByteString (ByteString)
import qualified Data.Attoparsec.ByteString.Char8 as P

value :: ByteString -> a -> P.Parser a
value match val = P.string match >> return val

notFound :: P.Parser ByteString
notFound = P.string "NOT_FOUND\r\n"

eol :: P.Parser ByteString
eol = P.string "\r\n"
