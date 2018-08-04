{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString
import Beanstalkd.Common (Job)
import Beanstalkd.System (withConn)
import Beanstalkd.Jobs.Put (puts)

-- main :: IO
main = withConn "localhost" "1130" $ puts (Job "Hello, World!")
