module Info where

info = "Smoke is a micro project manager for Haskell\n\
\\n\
\Usage:\n\
\    smoke [subcommand]\n\
\\n\
\Subcommands:\n\
\   run              Run project\n\
\   crun             Compile and run project\n\
\   compile          Compile project \n\
\   new <name>       Create new project\n"

-- compile string
-- 

text :: IO ()
text = putStrLn info
