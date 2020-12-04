module Subcmd where

import Error
import String
import System.Directory
import System.Process
import System.Info

config = "ghc.conf"
mainhs = "src/Main.hs"


-- SUBCOMMAND RUN
run :: [String] -> IO ()
run args =
    doesFileExist config >>= \case
    False -> Error.missing config
    True ->
        readFile config >>= \ccmd ->
        let binary = head $ tail $ tail $ words ccmd in
        doesFileExist binary >>= \case
        False -> Error.missing binary   
        True -> callProcess binary args


-- SUBCOMMAND CRUN
crun :: [String] -> IO ()
crun args =  
    checkFiles >>= \case
    False -> return ()
    True -> doesDirectoryExist "bin" >>= \case
        False -> createDirectory "bin" >> startCompile >> run args
        True -> startCompile >> run args


-- SUBCOMMAND COMPILE
release :: IO ()
release = 
    checkFiles >>= \case
    False -> return ()
    True -> doesDirectoryExist "bin" >>= \case
        False -> createDirectory "bin" >> startCompile 
        True -> startCompile

checkFiles :: IO Bool
checkFiles = 
    doesFileExist mainhs >>= \mainhsExists ->
    doesFileExist config >>= \configExists ->    
        case (mainhsExists, configExists) of
            (False, _) -> Error.missing mainhs >> return False
            (_, False) -> Error.missing config >> return False
            otherwise -> return True

startCompile :: IO ()
startCompile =
    readProcess "ghc" ["--version"] "" >>= \out ->
        case head $ tail $ words out of
            "Glorious" -> readFile config >>= \ccmd -> callProcess "ghc" ("-O2":(words ccmd))
            otherwise -> Error.ghc


-- SUBCOMMAND NEW
new :: String -> IO ()
new name = 
    doesDirectoryExist name >>= \case
    True -> Error.exists name
    False ->   
        makeAbsolute name >>= \path ->
        createDirectory name >>
        setCurrentDirectory path >>
        createDirectory "src" >>
            
        writeFile mainhs String.mainFile >>
        writeFile "README.md" ("# " ++ name) >>
        writeConfig name >> gitInit

writeConfig :: String -> IO ()
writeConfig name =
    case os of
        "mingw32" -> writeFile config (String.configFile $ name ++ ".exe")
        otherwise -> writeFile config (String.configFile name)

gitInit :: IO ()
gitInit =
    readProcess "git" ["--version"] "" >>= \out ->
        case head $ tail $ words out of
            "version" -> callProcess "git" ["init", "-q"] >> writeFile ".gitignore" "/bin"
            otherwise -> Error.git 


-- SUBCOMMAND RESTORE
restore :: String -> IO ()
restore name = writeConfig name


-- SUBCOMMAND COUNT
count :: String -> IO ()
count sourceFolder =
    doesDirectoryExist sourceFolder >>= \case
    False -> Error.missing sourceFolder
    True -> extractContents sourceFolder >>= \result ->
        putStrLn $ "total line count: " ++ (show result)

extractContents :: String -> IO Int
extractContents folder =
    listDirectory folder >>= \contents ->
        case null contents of
            True -> return 0
            False -> handleContents contents folder 0

handleContents :: [String] -> String -> Int -> IO Int
handleContents [] _ result = return result
handleContents (x:xs) path result =
    let newPath = path ++ x in
    doesFileExist newPath >>= \case
    False ->
        extractContents newPath >>= \lineCount ->
        handleContents xs path (result+lineCount)
    True ->
        putStrLn newPath >>
        readFile newPath >>= \contents ->
        handleContents xs path result >>= \lineCount ->
        return $ (length $ lines contents) + lineCount


-- SUBCOMMAND PATH
fetchExecPath :: IO ()
fetchExecPath = findExecutable "smoke" >>= \case
    Just executable -> putStrLn executable
    Nothing -> Error.execPath
