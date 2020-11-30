module Main where

import Info
import Error
import Subcmd
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    
    case length args of
        0 -> Info.text

        1 -> case head args of
            "run" -> Subcmd.run 
            otherwise -> Error.unknown $ head args
            
        2 -> case head args of
            "new" -> Subcmd.new $ last args
            "build" -> Subcmd.build $ last args
            otherwise -> Error.unknown $ head args

        otherwise -> Error.many
