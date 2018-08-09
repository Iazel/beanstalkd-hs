{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Beanstalkd.Common 
    ( module Beanstalkd.Common
    , module Beanstalkd.Error
    )
    where

import Prelude hiding (id, length)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64, Word32)
import Data.Aeson (FromJSON(..), withText)
import qualified Data.Aeson.Types as A
import Data.ByteString (ByteString, length)
import Network.Socket (Socket)
import Beanstalkd.Error (Error)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString, Builder, intDec, string7)
import Beanstalkd.Internals.ToByteStringBuilder (ToByteStringBuilder)
import Data.Attoparsec.ByteString.Char8 (Parser, many1, digit)

newtype ID = ID Word64
    deriving (Show, FromJSON, ToByteStringBuilder)

parseId :: Parser ID
parseId = do
    id <- many1 digit
    return $ ID (read id)

newtype Priority = Priority Word32
    deriving (Show, FromJSON, ToByteStringBuilder)

newtype Seconds = Seconds Word32
    deriving (Show, FromJSON, ToByteStringBuilder)

newtype TTR = TTR Word32
    deriving (Show, FromJSON, ToByteStringBuilder)

newtype Job = Job ByteString
    deriving (Show, ToByteStringBuilder)

jsonParser :: (ByteString -> a) -> A.Value -> A.Parser a
jsonParser constr = withText "string" (return . constr . encodeUtf8)

instance FromJSON Job where
    parseJSON = jsonParser Job

newtype Tube = Tube ByteString
    deriving (Show, ToByteStringBuilder)

instance FromJSON Tube where
    parseJSON = jsonParser Tube

newtype Conn = Conn Socket

type Delay = Seconds
type Count = Word32
type Amount = Word32

type Response a = Either Error a

sep :: Builder
sep = string7 "\r\n"

toBStr :: Builder -> ByteString
toBStr = toStrict . toLazyByteString 

jobLen :: Job -> Builder
jobLen (Job body) = intDec $ length body

jobStatsLabel :: String -> String
jobStatsLabel "timeLeft" = "time-left"
jobStatsLabel x = x
