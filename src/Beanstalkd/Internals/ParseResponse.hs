module Beanstalkd.Internals.ParseResponse where

import Beanstalkd.Error (Error, parseError)
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P

class ParseResponse a where
    parser :: Parser a

    parse :: ByteString -> Either Error a
    parse msg = toEither $ P.maybeResult $ P.parse parser msg
        where
        toEither Nothing  = Left $ parseError msg
        toEither (Just a) = Right a
