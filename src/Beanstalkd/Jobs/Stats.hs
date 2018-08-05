module Beanstalkd.Jobs.Stats where

data State = Ready | Delayed | Reserved | Buried deriving Show
data JobStats = JobStats
    { id :: ID
    , tube :: Tube
    , state :: State
    , priority :: Priority
    , age :: Seconds
    , delay :: Seconds
    , ttr :: TTR
    , timeLeft :: Seconds
    , file :: Word32
    , reserves :: Count
    , timeouts :: Count
    , releases :: Count
    , buries   :: Count
    , kicks    :: Count
    }

data JobStatsResponse = Ok JobStats | NotFound deriving Show

instance ParseResponse JobStatsResponse where
    parser = notFound NotFound
        <|> P.string7 "OK " *> (Ok <$> parseStats) <* eol
        where
            parseStats = parseLen >>= P.take
            parseLen = P.decimal <* eol

jobStats :: ID -> Conn -> IO (Response JobStatsResponse)
jobStats id (Conn sock) =
    send sock request >> recv sock 1024 >>= parseWith (recv sock 4096)
    where
        request = toBStr $ B.string7 "stats-job " <> conv id <> sep
