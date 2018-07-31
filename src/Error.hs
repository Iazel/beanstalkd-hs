{-# LANGUAGE OverloadedStrings #-}
module Beanstalkd.Error where

import Beanstalkd.Internals.Parser (value)
import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import qualified Data.Attoparsec.ByteString.Char8 as P

data Error
    = OutOfMemory
    | InternalError
    | BadFormat
    | UnknownCommand
    | UnknownResponse ByteString
    deriving Show

parser = (value "OUT_OF_MEMORY\r\n"  OutOfMemory)
    <|> (value "INTERNAL_ERROR\r\n"  InternalError)
    <|> (value "BAD_FORMAT\r\n"      BadFormat)
    <|> (value "UNKNOWN_COMMAND\r\n" UnknownCommand)

parseError :: ByteString -> Error
parseError msg = withDefault (UnknownResponse msg) msg
    where
        withDefault :: Error -> ByteString -> Error
        withDefault def = maybe def id . maybeParse

        maybeParse :: ByteString -> Maybe Error
        maybeParse = P.maybeResult . (P.parse parser)
