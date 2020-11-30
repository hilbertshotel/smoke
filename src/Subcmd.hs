module Subcmd where

import Error
import System.Directory
import System.Process


-- SUBCOMMAND BUILD
build :: String -> IO ()
build name = do
    currentDir <- getCurrentDirectory
    fileExists <- doesFileExist (currentDir ++ "/src/Main.hs")
    dirExists <- doesDirectoryExist (currentDir ++ "/bin")

    case (fileExists, dirExists) of
        (False, _) -> Error.missing "`src/Main.hs`"
        (True, False) -> Error.missing "`bin` directory"
        otherwise -> do

            let c = "ghc -o bin/"++name++" -no-keep-hi-files -no-keep-o-files -i:src Main"         
            callCommand c
            return ()


-- SUBCOMMAND RUN
run :: IO ()
run = do
    currentDir <- getCurrentDirectory
    fileExists <- doesFileExist (currentDir ++ "/src/Main.hs")
    
    if not fileExists
        then Error.missing "`src/Main.hs`"
        else do

            callCommand "ghc -no-keep-hi-files -no-keep-o-files -i:src Main"
            let tempFile = currentDir ++ "/src/Main"
            tempExists <- doesFileExist tempFile
            
            if not tempExists
                then return ()
                else do
            
                    callCommand tempFile
                    removeFile (tempFile)
                    return ()
    

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
