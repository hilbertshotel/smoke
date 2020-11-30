module Info where

info = "Smoke is a micro project manager for Haskell\n\
\\n\
\Usage:\n\
\    smoke [subcommand]\n\
\\n\
\Subcommands:\n\
\    new <name>      Creates new Haskell project\n\
\    run             Runs current project\n\
\    build <name>    Compiles current project\n"

text :: IO ()
text = putStrLn info
