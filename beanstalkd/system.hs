module Beanstalkd.System where

import Utils.Types

data SystemCommand
	= Stats
	| ListTubes
	| TubeUsed
	| TubesWatched
	| Quit
	| PauseTube Tube Delay

data Response a
	= OK a

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
