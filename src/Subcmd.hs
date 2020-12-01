module Subcmd where

import Error
import String
import System.Directory
import System.Process


-- SUBCOMMAND RUN
run :: IO ()
run =
    readFile "Smoke.config" >>= \ccmd ->
    let path = "bin/" ++ (extractName ccmd False) in
    doesFileExist path >>= \case
    False -> Error.missing ("`" ++ path ++ "`")   
    True -> callCommand path

extractName :: String -> Bool -> String
extractName (' ':xs) True = ""
extractName (x:xs) True = x : extractName xs True 
extractName ('/':xs) _ = extractName xs True
extractName (x:xs) bool = extractName xs bool


-- SUBCOMMAND CRUN
crun :: IO ()
crun = compile >> run


-- SUBCOMMAND BUILD
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
    doesFileExist "Smoke.config" >>= \configExists ->    
        case (mainExists, configExists) of
            (False, _) -> Error.missing "`src/Main.hs`" >> return False
            (_, False) -> Error.missing "`Smoke.config`" >> return False
            otherwise -> return True

readConfig :: IO ()
readConfig =
    -- check config integrity
    readFile "Smoke.config" >>= \ccmd ->
    callCommand ccmd


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
        writeFile "Smoke.config" (String.config name) >>

        readProcess "git" ["--version"] "" >>= \out ->
            case length $ words out of
                3 -> callCommand "git init -q" >> writeFile ".gitignore" "/bin"
                otherwise -> Error.git 
