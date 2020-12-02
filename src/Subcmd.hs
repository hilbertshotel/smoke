module Subcmd where

import Error
import String
import System.Directory
import System.Process
import System.Info

configFile = "ghc.conf"


-- SUBCOMMAND RUN
run :: [String] -> IO ()
run args =
    doesFileExist configFile >>= \case
    False -> Error.missing configFile
    True ->
        readFile configFile >>= \ccmd ->
        let path = head $ tail $ tail $ words ccmd in
        doesFileExist path >>= \case
        False -> Error.missing path   
        True -> callProcess path args


-- SUBCOMMAND CRUN
crun :: [String] -> IO ()
crun args = 
    checkFiles >>= \case
    False -> return ()
    True -> doesDirectoryExist "bin" >>= \case
        False -> createDirectory "bin" >> readConfig >> run args
        True -> readConfig >> run args


-- SUBCOMMAND COMPILE
compile :: IO ()
compile = 
    checkFiles >>= \case
    False -> return ()
    True -> doesDirectoryExist "bin" >>= \case
        False -> createDirectory "bin" >> readConfig 
        True -> readConfig

checkFiles :: IO Bool
checkFiles = 
    doesFileExist "src/Main.hs" >>= \mainExists ->
    doesFileExist configFile >>= \configExists ->    
        case (mainExists, configExists) of
            (False, _) -> Error.missing "src/Main.hs" >> return False
            (_, False) -> Error.missing configFile >> return False
            otherwise -> return True

readConfig :: IO ()
readConfig =
    readFile configFile >>= \ccmd ->
    readProcess "ghc" ["--version"] "" >>= \out ->
        case head $ tail $ words out of
            "Glorious" -> callCommand ccmd
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
            
        writeFile "src/Main.hs" String.mainFile >>
        writeFile "README.md" ("# " ++ name) >>
        writeConfig name >> handleGit

writeConfig :: String -> IO ()
writeConfig name =
    case os of
        "mingw32" -> writeFile configFile (String.config $ name ++ ".exe")
        otherwise -> writeFile configFile (String.config name)

handleGit :: IO ()
handleGit =
    readProcess "git" ["--version"] "" >>= \out ->
        case head $ tail $ words out of
            "version" -> callCommand "git init -q" >> writeFile ".gitignore" "/bin"
            otherwise -> Error.git 


-- SUBCOMMAND RESTORE
restore :: String -> IO ()
restore name = writeConfig name


-- SUBCOMMAND COUNT
count :: IO ()
count =
    doesDirectoryExist "src" >>= \case
    False -> Error.missing "src/"
    True -> extractContents "src" >>= \count ->
        putStrLn $ "total line count: " ++ (show count)

extractContents :: String -> IO Int
extractContents path =
    listDirectory path >>= \contents ->
        case null contents of
            True -> return 0
            False -> handleContents contents path 0

handleContents :: [String] -> String -> Int -> IO Int
handleContents [] _ result = return result
handleContents (x:xs) path result =
    let newPath = path ++ "/" ++ x in
    doesFileExist newPath >>= \case
    False ->
        extractContents newPath >>= \count ->
        handleContents xs path (result+count)
    True ->
        putStrLn newPath >>
        readFile newPath >>= \contents ->
        handleContents xs path result >>= \count ->
        return $ (length $ lines contents) + count
