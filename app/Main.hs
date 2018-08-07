module Main where

import Data.ByteString.Char8
import Beanstalkd.Common
import Beanstalkd.System (withConn)
import Beanstalkd.Jobs.Put (put)
import Beanstalkd.Jobs.Reserve
import Beanstalkd.Jobs.Delete (delete)
-- import Beanstalkd.Jobs.Release (rels)
import Beanstalkd.Jobs.Stats (jobStats)

main :: IO ()
main = withConn "localhost" "11300" loop
    where 
        loop :: Conn -> IO ()
        loop conn = do
            -- put (Priority 100) (Seconds 1) (TTR 60) (Job $ pack "Hello, World!") conn >>= print
            resp <- reserve conn
            print resp
            jobStats (jobId resp) conn >>= print
            delete (jobId resp) conn >>= print

        jobId (Right (Reserved x _)) = x
        jobId _ = undefined
