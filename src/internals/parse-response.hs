module Beanstalkd.Internals.ParseResponse where

import Beanstalkd.Error (Error)
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly)
import qualified Data.Attoparsec.ByteString.Char8 as P

class ParseResponse a where
    parser :: Parser a

    parse :: ByteString -> Either Error a
    parse msg = case (parseOnly parser msg) of
        Left  _ -> parseError msg
        Right a -> a
