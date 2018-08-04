module Main where

import Data.ByteString.Char8
import Beanstalkd.Common (Job(..))
import Beanstalkd.System (connect)
import Beanstalkd.Jobs.Put (puts)
import Beanstalkd.Jobs.Reserve (reserve)

main :: IO ()
main = do
    conn <- connect "localhost" "11300" 
    puts (Job $ pack "Hello, World!") conn >>= print
    reserve conn >>= print

