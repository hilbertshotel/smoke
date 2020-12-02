module Main where

import String
import Error
import Subcmd
import System.Directory
import System.Environment


main :: IO ()
main = getArgs >>= \args ->
    
    case length args of
        0 -> String.info

        1 -> case head args of
            "run" -> Subcmd.run
            "crun" -> Subcmd.crun
            "compile" -> Subcmd.compile
            "count" -> Subcmd.count
            "help" -> String.help
            "new" -> Error.noname $ head args
            "restore" -> Error.noname $ head args
            otherwise -> Error.unknown $ head args
            
        2 -> case head args of
            "new" -> Subcmd.new $ last args
            "restore" -> Subcmd.restore $ last args 
            "run" -> Error.noargs $ head args
            "crun" -> Error.noargs $ head args
            "compile" -> Error.noargs $ head args
            "count" -> Error.noargs $ head args
            "help" -> Error.noargs $ head args
            otherwise -> Error.unknown $ head args

        otherwise -> Error.many
