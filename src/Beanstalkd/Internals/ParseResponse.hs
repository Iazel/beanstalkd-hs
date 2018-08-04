module Beanstalkd.Internals.ParseResponse where

import Beanstalkd.Error (Error, parseError)
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P

class ParseResponse a where
    parser :: Parser a

    parse :: ByteString -> Either Error a
    parse msg = toEither msg $ P.maybeResult $ P.parse parser msg

    parseWith :: Monad m => m ByteString -> ByteString -> m (Either Error a)
    parseWith source initial 
        = (P.parseWith source parser initial) >>= return . (toEither initial) . P.maybeResult

toEither :: ByteString -> Maybe b -> Either Error b
toEither msg Nothing = Left $ parseError msg
toEither _ (Just a)  = Right a
