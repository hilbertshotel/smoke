module Main where

import Info
import Error
import Subcmd
import System.Directory
import System.Environment


main :: IO ()
main = getArgs >>= \args ->
    getCurrentDirectory >>= \currentDir ->
    
    case length args of
        0 -> Info.text

        1 -> case head args of
            "run" -> Subcmd.run currentDir
            otherwise -> Error.unknown $ head args
            
        2 -> case head args of
            "new" -> Subcmd.new (last args) currentDir 
            "build" -> Subcmd.build (last args) currentDir
            otherwise -> Error.unknown $ head args

        otherwise -> Error.many
