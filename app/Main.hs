module Main where

import Data.ByteString.Char8
import Beanstalkd.Common (Job(..))
import Beanstalkd.System (withConn)
import Beanstalkd.Jobs.Put (puts)
import Beanstalkd.Jobs.Reserve
import Beanstalkd.Jobs.Delete (delete)
import Beanstalkd.Jobs.Release (rels)

main :: IO ()
main = withConn "localhost" "11300" $ \conn -> do
    -- puts (Job $ pack "Hello, World!") conn >>= print
    resp <- reserve conn
    print resp
    rels (jobId resp) conn >>= print
    reserve conn >>= print
    delete (jobId resp) conn >>= print
    where
        jobId (Right (Reserved x _)) = x
        jobId _ = undefined
