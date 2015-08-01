module Main where

import System.Posix.Process(executeFile)

main :: IO ()
main = executeFile "./Test/semantic.sh" False [] Nothing
