{-# LANGUAGE OverloadedStrings #-}
module Beanstalkd.Jobs where

import Utils.Types
import Data.Word (Word32)
import Network.Socket.ByteString (recv, send)
import Data.ByteString.Builder (toLazyByteString)

instance ParseResponse PutResponse where
    parse msg
        | isPrefixOf "INSERTED " msg = Inserted (extractId msg)
        | isPrefixOf "BURIED "   msg = Buried (extractId msg)
        | otherwise = case msg of
            "DRAINING\r\n"      -> Draining
            "JOB_TOO_BIG\r\n"   -> JobTooBig
            "EXPECTED_CRLF\r\n" -> ExpectedCRLF
            otherwise           -> Error $ parse msg

data ReserveResponse
	= TimedOut
	| DeadlineSoon
	| Reserved ID Job
    | Error Error

instance ParseResponse ReserveResponse where
    parse msg
        | isPrefixOf "RESERVED " msg = buildReserved msg
        | otherwise = case msg of
            "TIMED_OUT\r\n"     -> TimedOut
            "DEADLINE_SOON\r\n" -> DeadlineSoon
            otherwise           -> Error $ parse msg

data PeekResponse
	= Found ID Job
	| NotFound
    | Error Error

instance ParseResponse PeekResponse where
    parse "NOT_FOUND\r\n" = NotFound
    parse msg
        | isPrefixOf "FOUND " msg = buildFound msg
        | otherwise = Error $ parse msg

data KickResponse
	= Kicked Count
    | Error Error

instance ParseResponse KickResponse where
    parse msg
        | isPrefixOf "KICKED " msg = extractCount msg
        | otherwise = Error $ parse msg

data JobStatsResponse
	= OK JobStats
	| NotFound
    | Error Error

instance ParseResponse JobStatsResponse where
    parse "NOT_FOUND\r\n" = NotFound
    parse msg
        | isPrefixOf "OK " msg = buildStats msg
        | otherwise = Error $ parse msg

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
