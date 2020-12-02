module String where

info :: IO ()
info = putStrLn "Smoke is a micro project manager for Haskell\n\
\\n\
\Usage:\n\
\   smoke [subcommand]\n\
\\n\
\Subcommands:\n\
\   run             - Run project\n\
\   compile         - Compile project \n\
\   crun            - Compile and run project\n\
\   new <name>      - Create new project\n\
\   restore <name>  - Restore default GHC config\n\
\   count           - Total line count for src/\n\
\   help            - Project structure details\n\
\\n\
\Default compile string:\n\
\   ghc -o bin/<name> -no-keep-hi-files -no-keep-o-files -XLambdaCase -i:src Main\n"


config :: String -> String
config name = "ghc -o bin/" ++ name ++ " -no-keep-hi-files -no-keep-o-files -XLambdaCase -i:src Main"


mainFile :: String
mainFile = "module Main where\n\nmain :: IO ()\nmain = putStrLn \"Hello World\""


help :: IO ()
help = putStrLn "help will make you lazy"
