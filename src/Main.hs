module Main where

import String
import Error
import Subcmd
import System.Environment


main :: IO ()
main = getArgs >>= \args ->
    
    case length args of
        0 -> String.info

        1 -> case head args of
            -- subcommands
            "run" -> Subcmd.run []
            "crun" -> Subcmd.crun []
            "release" -> Subcmd.release
            "count" -> Subcmd.count "src/"
            "path" -> Subcmd.fetchExecPath
            "help" -> String.help
            -- errors
            "new" -> Error.noname "new"
            "restore" -> Error.noname "restore"
            otherwise -> Error.unknown $ head args
            
        2 -> case head args of
            -- subcommands
            "new" -> Subcmd.new $ last args
            "restore" -> Subcmd.restore $ last args 
            "run" -> Subcmd.run $ tail args
            "crun" -> Subcmd.crun $ tail args
            -- errors
            "release" -> Error.noargs "release"
            "count" -> Error.noargs "count"
            "path" -> Error.noargs "path"
            "help" -> Error.noargs "help"
            otherwise -> Error.unknown $ head args

        otherwise -> case head args of
            -- subcommands
            "run" -> Subcmd.run $ tail args
            "crun" -> Subcmd.crun $ tail args
            -- errors
            otherwise -> Error.many
