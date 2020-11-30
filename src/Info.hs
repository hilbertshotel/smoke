module Info where

info = "Smoke is a micro project manager for Haskell\n\
\\n\
\Usage:\n\
\    smoke [subcommand]\n\
\\n\
\Subcommands:\n\
\    new <name>      Create a new Haskell project\n\
\    run             Compile and run binary\n"

text :: IO ()
text = do
    putStrLn info
