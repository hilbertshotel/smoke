module Subcmd where

import Error
import System.Directory
import System.Process


-- SUBCOMMAND BUILD
build :: String -> String -> IO ()
build name currentDir = doesFileExist (currentDir ++ "/src/Main.hs") >>= \fileExists ->
    doesDirectoryExist (currentDir ++ "/bin") >>= \dirExists ->
        case (fileExists, dirExists) of
            (False, _) -> Error.missing "`src/Main.hs`"
            (True, False) -> Error.missing "`bin` directory"
            otherwise ->
                callCommand ("ghc -o bin/"++name++" -no-keep-hi-files -no-keep-o-files -i:src Main")  


-- SUBCOMMAND RUN
run :: String -> IO ()
run currentDir = doesFileExist (currentDir ++ "/src/Main.hs") >>= \fileExists ->
    if not fileExists then Error.missing "`src/Main.hs`" else
        let tempFile = currentDir ++ "/src/Main" in
            callCommand "ghc -no-keep-hi-files -no-keep-o-files -i:src Main" >>
                callCommand tempFile >> removeFile tempFile    
            

-- SUBCOMMAND NEW
new :: String -> String -> IO ()
new name currentDir = doesDirectoryExist name >>= \dirExists ->
    if dirExists then Error.exists name else do
        let path = currentDir ++ "/" ++ name
            main = "module Main where\n\nmain :: IO ()\nmain = do\n    putStrLn \"Hello World\""

        createDirectoryIfMissing True (path ++ "/src")
        setCurrentDirectory path
        createDirectory "bin"
        writeFile (path ++ "/src/Main.hs") main 
        writeFile (path ++ "/.gitignore") "/bin"
        writeFile (path ++ "/README.md") ("# " ++ name)
        callCommand "git init -q"
