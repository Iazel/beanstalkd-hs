module Main where

import Data.ByteString.Char8
import Beanstalkd.Common (Job(..))
import Beanstalkd.System (withConn)
import Beanstalkd.Jobs.Put (puts)

main :: IO ()
main = do
    resp <- withConn "localhost" "11300" $ puts (Job $ pack "Hello, World!")
    print resp
