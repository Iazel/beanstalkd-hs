module Beanstalkd.System where

import Utils.Types
import qualified Network.Socket
import qualified Control.Exception.Base as E

data SystemCommand
	= Stats
	| ListTubes
	| TubeUsed
	| TubesWatched
	| Quit
	| PauseTube Tube Delay

data Response a
	= OK a

type Hostname = String
type Port = String

type ListTubeResponse
	= Response [Tube]

type TubeUsedResponse
	= Response Tube

type TubesWatchedResponse
	= Response [Tube]

type StatsResponse
	= Response SystemStats


type Bytes = Word32
data SystemStats = Stats {
	id :: ByteString,
	hostname :: ByteString,
	pid :: PID,
	version :: ByteString,

	urgentJobs :: Count,
	readyJobs :: Count,
	reservedJobs :: Count,
	buriedJobs :: Count,
	totalJobs :: Count,
	maxJobSize :: Count,

	tubes :: Count,
	connections :: Count,
	producers :: Count,
	workers :: Count,
	waiting :: Count,
	totalConnections :: Count,

	cmdPut :: Count,
	cmdPeek :: Count,
	cmdPeekReady :: Count,
	cmdPeekDelayed :: Count,
	cmdPeekBuried :: Count,
	cmdReserve :: Count,
	cmdUse :: Count,
	cmdWatch :: Count,
	cmdIgnore :: Count,
	cmdDelete :: Count,
	cmdRelease :: Count,
	cmdBury :: Count,
	cmdKick :: Count,
	cmdStats :: Count,
	cmdStatsJob :: Count,
	cmdStatsTube :: Count,
	cmdListTube :: Count,
	cmdListTubeUsed :: Count,
	cmdListTubeWatched :: Count,
	cmdPauseTube :: Count,

	rusageUser   :: UTime,
	rusageSystem :: UTime,
	upTime :: Seconds,

	binlogOldest :: Word32,
	binlogCurrent :: Word32,
	binlogMaxSize :: Bytes,
	binlogRecordsWritten :: Count,
	binlogRecordsMigrated :: Count,
}

connect Hostname -> Port -> IO Conn
connect host port = do
    addr <- getAddr
    sock <- Socket.socket
        (Socket.addrFamily addr)
        (Socket.addrSocketType addr)
        (Socket.addrProtocol addr)
    connect sock (addrAddress addr)
    return (Conn sock)
    where
        getAddr = do
            let hints = Socket.defaultHints { addrSocketType = Stream }
            addr:_ <- Socket.getAddrInfo (Just hints) (Just host) (Just port)
            return addr

close :: Conn -> IO ()
close (Conn socket) = Socket.close socket

withConn :: Hostname -> Port -> (Conn -> IO a) -> IO a
withConn host port f = E.bracket (connect host port) close f
