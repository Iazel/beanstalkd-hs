{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Beanstalkd.Jobs.Stats where

import Prelude hiding (id)
import Data.Text (unpack)
import Data.Aeson (FromJSON(..))
import qualified Data.Aeson.TH as ATH
import Data.Aeson.Types as A
import qualified Data.Yaml as Y
import Beanstalkd.Common
import Beanstalkd.Internals.Parser (notFound, eol)
import Beanstalkd.Internals.ParseResponse
import Beanstalkd.Internals.ToByteStringBuilder (conv)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Control.Applicative ((<|>), (*>), (<*))
import qualified Data.ByteString.Builder as B
import Network.Socket.ByteString (send, recv)
import Data.Word (Word32)

data State = Ready | Delayed | Reserved | Buried deriving Show

instance FromJSON State where
    parseJSON = A.withText "string" (parseState . unpack)
        where
            parseState :: String -> A.Parser State
            parseState s = case s of
                "ready"    -> return Ready
                "delayed"  -> return Delayed
                "reserved" -> return Reserved
                "buried"   -> return Buried
                _          -> fail ("Invalid value for State: " ++ s)

data JobStats = JobStats
    { id       :: ID
    , tube     :: Tube
    , state    :: State
    , pri      :: Priority
    , age      :: Seconds
    , delay    :: Seconds
    , ttr      :: TTR
    , timeLeft :: Seconds
    , file     :: Word32
    , reserves :: Count
    , timeouts :: Count
    , releases :: Count
    , buries   :: Count
    , kicks    :: Count
    }
    deriving Show

$(ATH.deriveFromJSON 
    ATH.defaultOptions { ATH.fieldLabelModifier = jobStatsLabel }
    ''JobStats)

data JobStatsResponse
    = Ok JobStats 
    | InvalidYaml Y.ParseException
    | NotFound 
    deriving Show

instance ParseResponse JobStatsResponse where
    parser = notFound NotFound
        <|> P.string "OK " *> parseStats <* eol
        where
            parseStats = (resp . Y.decodeEither') <$> (parseLen >>= P.take)
            parseLen = P.decimal <* eol

            resp (Left e) = YamlError e
            resp (Right s) = Ok s

jobStats :: ID -> Conn -> IO (Response JobStatsResponse)
jobStats jid (Conn sock) =
    send sock request >> recv sock 1024 >>= parseWith (recv sock 4096)
    where
        request = toBStr $ B.string7 "stats-job " <> conv jid <> sep
