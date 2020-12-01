module Subcmd where

import Error
import System.Directory
import System.Process


-- SUBCOMMAND RUN
run :: IO ()
run = doesFileExist "bin/temp" >>= \case
    False -> return ()
    True -> 


-- SUBCOMMAND CRUN
crun :: IO ()
crun = checkFileAndFolder >>= \case
    False -> return ()
    True -> 
        callCommand "ghc -o bin/temp -no-keep-hi-files -no-keep-o-files -XLambdaCase -i:src Main" >>
        doesFileExist "bin/temp" >>= \case
        True -> callCommand "bin/temp"
        False -> return ()


-- SUBCOMMAND BUILD
compile :: IO () -- REMOVE TEMP FILE?
compile = checkFileAndFolder >>= \case
    True -> callCommand ("ghc -o bin/"++name++" -no-keep-hi-files -no-keep-o-files -XLambdaCase -i:src Main")  
    False -> return ()


-- SUBCOMMAND NEW
new :: String -> IO ()
new name = doesDirectoryExist name >>= \case
    True -> Error.exists name
    False -> createNewProject name

createNewProject :: String -> IO () -- DO
createNewProject name = makeAbsolute name >>= \path ->
    createDirectory name >> setCurrentDirectory path >>
    createDirectory "src" >> createDirectory "bin" >>
    let main = "module Main where\n\nmain :: IO ()\nmain = putStrLn \"Hello World\"" in
        writeFile ("src/Main.hs") main >>
        writeFile (".gitignore") "/bin" >>
        writeFile ("README.md") ("# " ++ name) >>
        callCommand "git init -q"

        writeFile("Smoke.toml")
    -- MAKE Smoke.toml file to read name from and create in Main.hs bin/ ++ name and pass to run and crun


-- UTILS
checkFileAndFolder :: IO Bool
checkFileAndFolder = doesFileExist "src/Main.hs" >>= \fileExists ->
    doesDirectoryExist "bin" >>= \dirExists ->
        case (fileExists, dirExists) of
            (False, _) -> Error.missing "`src/Main.hs`" >> return False
            (_,False) -> Error.missing "`bin` directory" >> return False
            otherwise -> return True
