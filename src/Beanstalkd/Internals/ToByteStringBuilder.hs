module Beanstalkd.Internals.ToByteStringBuilder where

import Data.Word (Word64, Word32)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, word64Dec, word32Dec, byteString)

class ToByteStringBuilder a where
    conv :: a -> Builder

instance ToByteStringBuilder ByteString where
    conv = byteString

instance ToByteStringBuilder Word32 where
    conv = word32Dec

instance ToByteStringBuilder Word64 where
    conv = word64Dec
