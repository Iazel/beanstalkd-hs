{-# LANGUAGE OverloadedStrings #-}
module Beanstalkd.Jobs where

import Utils.Types
import Data.Word (Word32)
import Network.Socket.Bytestring (recv, send)
import Data.ByteString.Builder (toLazyByteString)

data PutResponse
	= Inserted ID
	| Buried ID
	| JobTooBig
	| Draining
	| ExpectedCRLF
    | Error Error

instance ParseResponse PutResponse where
    parse msg
        | (take 4 msg) == "INSERT " = Inserted (extractId msg)
        | (take 4 msg) == "BURIED " = Buried (extractId msg)
        | otherwise                 = case msg of
            "DRAINING\r\n"      -> Draining
            "JOB_TOO_BIG\r\n"   -> JobTooBig
            "EXPECTED_CRLF\r\n" -> ExpectedCRLF
            otherwise           -> Error $ parse msg

data ReserveResponse
	= TimedOut
	| DeadlineSoon
	| Reserved ID Job
    | Error Error

data PeekResponse
	= Found ID Job
	| NotFound
    | Error Error

data KickResponse
	= Kicked Count
    | Error Error

data JobStatsResponse
	= OK JobStats
	| NotFound
    | Error Error

data State = Ready | Delayed | Reserved | Buried
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

put :: Priority -> Delay -> TRR -> Conn -> Job -> IO PutResponse
put prio delay trr (Conn sock) job = do
    send request
    response <- recv sock
    return $ parse response
    where request = toLazyByteString
            $  (conv prio)
            <> (conv delay) 
            <> (conv trr)
            <> (jobLen job) 
            <> sep 
            <> (conv job)
            <> sep

puts :: Conn -> Job -> IO PutResponse
puts = put (Priority 100) (Seconds 0) (TRR 60)

reserve :: Conn -> IO ReserveResponse
reserveWithTimeout :: Conn -> Seconds -> IO ReserveResponse

touch :: Conn -> ID -> IO GenericResponse

delete :: Conn -> ID -> IO GenericResponse

release :: Conn -> Priority -> Delay -> ID -> IO GenericResponse

bury :: Conn -> Priority -> ID -> IO GenericResponse

kick :: Conn -> Amount -> IO KickResponse
kickJob :: Conn -> ID -> IO KickResponse

statsJob :: Conn -> ID -> IO JobStatsResponse

peek :: Conn -> ID -> IO PeekResponse
peekReady :: Conn -> IO PeekResponse
peekDelayed :: Conn -> IO PeekResponse
peekBuried :: Conn -> IO PeekResponse
