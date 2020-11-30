module Subcmd where

import Error
import System.Directory
import System.Process


-- SUBCOMMAND RUN
run :: IO ()
run = do
    currentDir <- getCurrentDirectory
    fileExists <- doesFileExist (currentDir ++ "/src/Main.hs")
    dirExists <- doesDirectoryExist (currentDir ++ "/bin")

    case (fileExists, dirExists) of
        (False, _) -> Error.missing "`src/Main.hs`"
        (True, False) -> Error.missing "`bin` directory"
        otherwise -> do

            let name = fromPathExtractName currentDir
            let bin = "../bin/" ++ name
            let crun = "cd src; ghc -o "++bin++" -no-keep-hi-files -no-keep-o-files Main.hs; cd ..; "++bin
         
            callCommand crun
            return ()
    
fromPathExtractName :: String -> String
fromPathExtractName currentDir = last path 
    where path = words [if c == '/' then ' ' else c | c <- currentDir]


-- SUBCOMMAND NEW
new :: String -> IO ()
new name = do
    dirExists <- doesDirectoryExist name
    if dirExists
        then Error.exists name
        else createNewProject name
        
createNewProject :: String -> IO ()
createNewProject name = do
    currentDir <- getCurrentDirectory
    let path = currentDir ++ "/" ++ name

    createDirectoryIfMissing True (path ++ "/src")
    setCurrentDirectory path
    createDirectory "bin"

    let main = "module Main where\n\nmain :: IO ()\nmain = do\n    putStrLn \"Hello World\""
    writeFile (path ++ "/src/Main.hs") main 
    writeFile (path ++ "/.gitignore") "/bin"
    writeFile (path ++ "/README.md") ("# " ++ name)
    
    callCommand "git init -q"
    return ()
