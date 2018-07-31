module Beanstalkd.Internals.ToByteStringBuilder where
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, word32Dec, byteString)

class ToByteStringBuilder a where
    conv :: a -> Builder

instance ToByteStringBuilder ByteString where
    conv = byteString

instance ToByteStringBuilder Word32 where
    conv = word32Dec
