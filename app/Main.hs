module Main (main) where

import Lib
import Control.Monad.Logger

main :: IO ()
main = runStdoutLoggingT someFunc
