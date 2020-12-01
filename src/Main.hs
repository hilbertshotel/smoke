module Main where

import Info
import Error
import Subcmd
import System.Directory
import System.Environment


main :: IO ()
main = getArgs >>= \args ->
    
    case length args of
        0 -> Info.text

        1 -> case head args of
            "run" -> Subcmd.run
            "crun" -> Subcmd.crun
            "compile" -> Subcmd.compile
            "new" -> Error.new
            otherwise -> Error.unknown $ head args
            
        2 -> case head args of
            "run" -> Error.noargs $ head args
            "crun" -> Error.noargs $ head args
            "compile" -> Error.noargs $ head args
            "new" -> Subcmd.new $ last args 
            otherwise -> Error.unknown $ head args

        otherwise -> Error.many
