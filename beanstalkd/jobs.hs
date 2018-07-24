module Beanstalkd.Jobs where

import Utils.Types
import Data.Word (Word32)

data JobCommand
	= Put Priority Delay TTR Job
	| Reserve
	| ReserveWithTimeOut Seconds
	| Delete ID
	| Release ID Priority Delay
	| Bury ID Priority
	| Touch ID
	| Kick Amount -- Max number of jobs to kick
	| KickJob ID
	| Stats ID
	| Peek ID
	| PeekReady
	| PeekDelayed
	| PeekBuried

data PutResponse
	= Inserted ID
	| Buried ID
	| JobTooBig
	| Draining
	| ExpectedCRLF

data ReserveResponse
	= TimedOut
	| DeadlineSoon
	| Reserved ID Job

data PeekResponse
	= Found ID Job
	| NotFound

data KickResponse
	= Kicked Count

data JobStatsResponse
	= OK JobStats
	| NotFound

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
