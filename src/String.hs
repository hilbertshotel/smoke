module String where

info :: IO ()
info = putStrLn "Smoke is a micro project manager for Haskell\n\
\\n\
\Usage:\n\
\   smoke [subcommand]\n\
\\n\
\Subcommands:\n\
\   new <name>      - Create new project\n\
\   run             - Run project (if compiled)\n\
\   crun            - Compile and run project\n\
\   release         - Compile project with -O2 optimization \n\
\   restore <name>  - Restore default GHC config string\n\
\   count           - Total line count for src/ folder\n\
\   path            - Find path of executable\n\
\   help            - Helpful and honest opinion\n\
\\n\
\Default GHC config string:\n\
\   src/Main.hs -o bin/<name> -i:src -no-keep-hi-files -no-keep-o-files -XLambdaCase\n\
\\n\
\Compilation info:\n\
\   - src/Main.hs is the entry point\n\
\   - -o bin/ is the compilation folder\n\
\   - both are hardcoded and necessary for Smoke to function properly\n\
\   - change <name> manually if you want to rename the binary output\n\
\   - edit -i:src if you want the compiler to include other source folders (-i:src:pkg)\n\
\   - edit ghc.conf file to add other necessary compile options\n"

configFile :: String -> String
configFile name = "src/Main.hs -o bin/" ++ name ++ " -i:src -no-keep-hi-files -no-keep-o-files -XLambdaCase"

mainFile :: String
mainFile = "module Main where\n\nmain :: IO ()\nmain = putStrLn \"Hello World\""

help :: IO ()
help = putStrLn "cremation grounds"

newProject :: String -> IO ()
newProject name = putStrLn $ "      Project \x1b[32m" ++ name ++ "\x1b[0m initialized"
