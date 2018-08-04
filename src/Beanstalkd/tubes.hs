module Beanstalkd.Tube where

import Utils.Types
import Data.ByteString (ByteString)

data TubeCommand
	= Stats Tube
	| Use Tube
	| Watch Tube
	| Ignore Tube

data WatchResponse
	= Watching Count

data IgnoreResponse
	= Watching Count
	| OnlyTubeInList

data StatsResponse
	= OK TubeStats
	| NotFound

data TubeStats = Stats
	{ name :: ByteString
	, urgentJobs :: Count
	, readyJobs :: Count
	, reservedJobs :: Count
	, buriedJobs :: Count
	, totalJobs :: Count
	, openConnections :: Count
	, waiting :: Count
	, wachting :: Count
	, pausedFor :: Seconds
	, cmdDelete :: Count
	, cmdPause :: Count
	, resumeIn :: Seconds
	}
